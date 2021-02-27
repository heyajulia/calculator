use v6;
use strict;

# TODO: Add a "Hat" token for "^" so we don't have to reuse "StarStar".
#| The different kinds of tokens.
enum TokenType <ParenOpen ParenClose Number StarStar Star Slash Plus Minus>;

#| A token that does not have a changing lexeme.
role Token {
    has TokenType $.type;
}

#| Numbers are special: they do have a changing lexeme (the textual
#| representation of its value).
class NumberToken does Token {
    has Str $.lexeme;

    submethod BUILD(Str :$!lexeme) {
        $!type = TokenType::Number;
    }
}

# TODO: See if we can get rid of this.
#| The "base class" for different expressions: literals, unary expressions,
#| and binary expressions.
role Expr {}

#| Literal respresents a value that directly appears in the source code, like
#| 123 or 456.
class Literal does Expr {
    has Num $.value;
}

#| Unary represents a unary expression, like -123.
class Unary does Expr {
    has Token $.operator;
    has Expr $.operand;
}

#| Represents a binary expression, like 123 + 456.
class Binary does Expr {
    has Expr $.left;
    has Token $.operator;
    has Expr $.right;
}

#| The lexer turns a string of "source code" (an arithmetic expression) into
#| a sequence of tokens.
class Lexer {
    has Str $!source;
    has UInt $!pos;

    # FIXME: `source` is a kwarg, which means we have to type
    # `Lexer.new(source => $source)`, which is noisy. It would be better if we
    # could just write `Lexer.new($source)` instead.
    submethod BUILD(Str :$!source) {
        $!pos = 0;
    }

    #| Returns an array of Tokens.
    method lex(--> Array) {
        # FIXME: It would probably be better to use a generator, but that seems
        # to involve quite a lot of boilerplate in Raku.
        my Token @tokens = [];

        # FIXME: `while (!(self!is-at-end()))` reads poorly, but it seems like
        # Raku also has a unary prefix `not` operator. We should consider
        # switching to that iff it's `!` by a different name:
        #
        #     while (not self!is-at-end()) { … }
        #
        # (P.S.: I know `while (!(self!is-at-end()))` could be written as
        # `while (!self!is-at-end())`, but that looks even stranger to me
        # because of the `!self!` part.
        while (!(self!is-at-end())) {
            self!eat-whitespace();

            my Str $current = self!current();

            if ($current eq "(") {
                @tokens.push(Token.new(type => TokenType::ParenOpen));

                self!advance();
            } elsif ($current eq ")") {
                @tokens.push(Token.new(type => TokenType::ParenClose));

                self!advance();
            } elsif ($current eq "*") {
                self!advance();

                my Str $next = self!current();

                # Handling "*" requires a little extra care: sometimes, there
                # will be another "*" right after it. Then, it's a
                # StarStar token. Else, it's a Star.
                if ($next eq "*") {
                    self!advance();

                    @tokens.push(Token.new(type => TokenType::StarStar));
                } else {
                    @tokens.push(Token.new(type => TokenType::Star));
                }
            } elsif ($current eq "^") {
                # HACK: We treat "^" as if it's "**", without a separate
                # TokenType.

                self!advance();

                @tokens.push(Token.new(type => TokenType::StarStar));
            } elsif ($current eq "/") {
                @tokens.push(Token.new(type => TokenType::Slash));

                self!advance();
            } elsif ($current eq "+") {
                @tokens.push(Token.new(type => TokenType::Plus));

                self!advance();
            } elsif ($current eq "-") {
                @tokens.push(Token.new(type => TokenType::Minus));

                self!advance();
            } else {
                # TODO: Extend this to handle decimal numbers.
                if (self!current() ~~ /\d/) {
                    my UInt $start-pos = $!pos;

                    while (self!current() ~~ /\d/) {
                        self!advance();
                    }

                    my UInt $token-length = $!pos - $start-pos;

                    @tokens.push(NumberToken.new(lexeme => $!source.substr($start-pos, $token-length)));
                } else {
                    die "Syntax error: don't know how to parse '{self!current()}'"
                }
            }
        }

        return @tokens;
    }

    #| Returns True if we've reached the end of the input, False otherwise.
    method !is-at-end(--> Bool) { $!pos >= $!source.chars }

    #| Returns the character at the current position.
    method !current(--> Str) {
        if (self!is-at-end) {
            return "\0";
        }

        return $!source.substr($!pos, 1);
    }

    #| Increments position by 1.
    method !advance() { $!pos += 1; }

    #| Advances while current is a whitespace character.
    method !eat-whitespace() {
        while (self!current() ~~ /\s/) {
            self!advance();
        }
    }
}

#| The parser turns a sequence of tokens into an Expr (which may include nested
#| sub-expressions).
class Parser {
    has Token @!tokens;
    has UInt $!pos;

    submethod BUILD(Token :@!tokens) {
        $!pos = 0;
    }

    method parse(--> Expr) {
        return self!expression();
    }

    #| Parses a flat sequence of + and - operations.
    #|
    #| Grammar:
    #|
    #|     expression : factor (("+" | "-") factor)* ;
    method !expression(--> Expr) {
        my Expr $expr = self!factor();

        while (self!match(TokenType::Plus, TokenType::Minus)) {
            my Token $operator = self!previous();
            my Expr $right = self!factor();

            $expr = Binary.new(left => $expr, operator => $operator, right => $right);
        }

        return $expr;
    }

    #| Parses a flat sequence of * and / operations.
    #|
    #| Grammar:
    #|
    #|     factor : power (("*" | "/") power)* ;
    method !factor(--> Expr) {
        my Expr $expr = self!power();

        while (self!match(TokenType::Star, TokenType::Slash)) {
            my Token $operator = self!previous();
            my Expr $right = self!power();

            $expr = Binary.new(left => $expr, operator => $operator, right => $right);
        }

        return $expr;
    }

    #| Parses a unary operator followed by one or more exponentiation operations.
    #|
    #| Grammar:
    #|
    #|     power : unary ("**" power)* ;
    method !power(--> Expr) {
        my Expr $expr = self!unary();

        while (self!match(TokenType::StarStar)) {
            my Token $operator = self!previous();
            my Expr $right = self!power();

            $expr = Binary.new(left => $expr, operator => $operator, right => $right);
        }

        return $expr;
    }

    #| Parses a series of unary minus operations or a primary expression.
    #|
    #| Gramamr:
    #|
    #|     unary : "-" unary
    #|           | primary ;
    method !unary(--> Expr) {
        if (self!match(TokenType::Minus)) {
            return Unary.new(operator => self!previous(), operand => self!unary());
        }

        return self!primary();
    }

    #| Parses a primary expression: a number or an expression enclosed in
    #| parentheses.
    #|
    #| Grammar:
    #|
    #|     primary : NUMBER
    #|             | "(" expression ")" ;
    method !primary(--> Expr) {
        if (self!match(TokenType::Number)) {
            my NumberToken $number = self!previous();

            return Literal.new(value => $number.lexeme.Num);
        }

        if (self!match(TokenType::ParenOpen)) {
            my Expr $expr = self!expression();

            self!expect(TokenType::ParenClose);

            return $expr;
        }
    }

    #| Returns the previously handled token. Returns Nil when $!pos is 0.
    method !previous(--> Token) {
        if ($!pos == 0) {
            return Nil;
        }

        return @!tokens[$!pos - 1];
    }

    #| Returns True if we've reached the token array, False otherwise.
    method !is-at-end(--> Bool) { $!pos >= @!tokens.elems }

    #| Increments position by 1.
    method !advance() { $!pos += 1; }

    #| Returns the token currently being handled, or Nil if we're past the end
    #| of the token array.
    method !current(--> Token) {
        if (self!is-at-end()) {
            return Nil;
        }

        return @!tokens[$!pos];
    }

    #| Returns True and increments the position if the token currently being
    #| handled is one of several passed to it, False otherwise.
    method !match(|types where .all ~~ TokenType --> Bool) {
        if (self!current() ~~ Nil) {
            return False;
        }

        for types -> $type {
            if (self!current().type == $type) {
                self!advance();

                return True;
            }
        }

        return False;
    }

    #| Advances the token stream if the type of the token currently being
    #| handled is equal to type. Throws an error otherwise.
    method !expect(TokenType $type --> Token) {
        if (!(self!current().type == $type)) {
            die "Syntax error: expected $type but got {self!current().type}";
        }

        my Token $token = self!current();

        self!advance();

        return $token;
    }
}

# And now we can *finally* answer the question: "do multimethods solve the
# Expression problem in an elegant way?".
#
# Read the code below and decide for yourself. Having used the Visitor pattern
# in both my Lox implementation and another language I'm hacking on called Ax,
# I believe the answer is: "yes, and it's not even funny."

multi evaluate(Literal $literal --> Num) {
    return $literal.value;
}

multi evaluate(Unary $unary --> Num) {
    if ($unary.operator.type == TokenType::Minus) {
        return -evaluate($unary.operand);
    }

    die "Syntax error: invalid unary operator {$unary.operator}";
}

multi evaluate(Binary $binary --> Num) {
    my Num $left = evaluate($binary.left);
    my Token $op = $binary.operator;
    my Num $right = evaluate($binary.right);

    if ($op.type == TokenType::StarStar) {
        return $left ** $right;
    } elsif ($op.type == TokenType::Star) {
        return $left * $right;
    } elsif ($op.type == TokenType::Slash) {
        return $left / $right;
    } elsif ($op.type == TokenType::Plus) {
        return $left + $right;
    } elsif ($op.type == TokenType::Minus) {
        return $left - $right;
    }

    die "Syntax error: invalid binary operator {$op}";
}

# For kicks (and because multimethods make it so darn easy),
# we'll also add an expression printer, that converts an expression to a
# string of valid Raku code.
#
# It's also great for debugging, because this string representation shows every
# parenthesis, which makes it easier to verify that we're handling
# precedence and associativity correctly.

multi print-expr(Literal $literal --> Str) {
    $literal.value.Str
}

multi print-expr(Unary $unary --> Str) {
    my Token $op = $unary.operator;
    my Str $symbol = do if ($op.type == TokenType::Minus) {
        "-"
    }

    "($symbol {print-expr($unary.operand)})"
}

multi print-expr(Binary $binary --> Str) {
    my Token $op = $binary.operator;

    # TODO: Add `$op.type == TokenType::Hat`.
    my Str $symbol = do if ($op.type == TokenType::StarStar) {
        "**"
    } elsif ($op.type == TokenType::Star) {
        "*"
    } elsif ($op.type == TokenType::Slash) {
        "/"
    } elsif ($op.type == TokenType::Plus) {
        "+"
    } elsif ($op.type == TokenType::Minus) {
        "-"
    }

    "({print-expr($binary.left)} $symbol {print-expr($binary.right)})"
}

# That's all there is to implementing an arithmetic expression evaluator using
# multimethods. Now, let's put everything together!

sub MAIN {
    my Str $source = "2 ^ 3 / 4 * 5 + 6 - 7";
    my Lexer $lexer = Lexer.new(source => $source);
    my Token @tokens = $lexer.lex();
    my Parser $parser = Parser.new(tokens => @tokens);
    my Expr $expr = $parser.parse();

    say "{print-expr($expr)} = {evaluate($expr)}";
}
