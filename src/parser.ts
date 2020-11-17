// @ts-ignore nearley import
import syntax from "./syntax.ne"
import { Parser, Grammar } from "nearley"
import { PurescriptEither } from "./types"
import type { Term } from "./ast"

const grammar = Grammar.fromCompiled(syntax)

export const parse = <T, E>({
  left,
  right
}: PurescriptEither<string, Term<T>, E>) => (input: string) => {
  try {
    const parser = new Parser(grammar)

    parser.feed(input)

    const result = parser.results[0]

    return right(result)
  } catch (err) {
    return left(err.message)
  }
}
