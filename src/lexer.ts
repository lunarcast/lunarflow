import moo from "moo"

export interface Position {
  line: number
  col: number
}

export interface Token {
  start: Position
  end: Position
  type?: string
  value: string
}

export const lexer = moo.compile({
  ws: /[ \t]+/,
  nl: { match: "\n", lineBreaks: true },
  lambdaChar: ["λ", "\\"],
  arrowChar: ["->", "."],
  lparan: "(",
  rparan: ")",
  identifier: {
    match: /[a-z_][a-z_0-9]*/
  }
})

export function tokenStart(token: moo.Token): Position {
  return {
    line: token.line,
    col: token.col - 1
  }
}

export function tokenEnd(token: moo.Token): Position {
  const lastNewLine = token.text.lastIndexOf("\n")

  if (lastNewLine !== -1) {
    throw new Error("Unsupported case: token with line breaks")
  }

  return {
    line: token.line,
    col: token.col + token.text.length - 1
  }
}

export function convertToken(token: moo.Token): Token {
  return {
    type: token.type,
    value: token.value,
    start: tokenStart(token),
    end: tokenEnd(token)
  }
}

export function convertTokenId(data: moo.Token[]): Token {
  return convertToken(data[0])
}
