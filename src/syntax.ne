@{%
const { lexer, tokenStart, convertTokenId } = require("./lexer.ts")
const { call, abstract, variable, abstractMany } = require("./ast.ts")
%}

@lexer lexer

expression 
    -> application {% id %}
    |  abstraction {% id %}

application 
    -> no_lambda_application  {% id %}
    | no_lambda_application __ abstraction
        {% ([left, _, right]) => call(left, right) %}

no_lambda_application 
    -> atom {% id %}
    |  no_lambda_application __ atom
        {% ([left, _, right]) => call(left, right) %}


atom
    -> "(" _ expression _ ")" {% d => d[2] %}
    |   variable {% id %} 

variable 
    -> identifier 
        {% ([d]) => variable(d.value) %}

abstraction
    -> lambdaChar lambdaArgs _ arrowChar _ expression
        {% d => abstractMany(d[1].map(a => a.value), d[5]) %}

lambdaArgs -> (_ identifier _ {% d => d[1] %}):+ {% id %}

# Lexing
arrowChar -> %arrowChar {% convertTokenId %}
lambdaChar -> %lambdaChar {% convertTokenId %}
identifier -> %identifier {% convertTokenId %}
__ -> %ws:+
_ -> %ws:*