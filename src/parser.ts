// @ts-ignore
import syntax from "./syntax.ne"
import { Parser, Grammar } from "nearley"

const parser = new Parser(Grammar.fromCompiled(syntax))

parser.feed("\\f a b. f b a \\l. l")
const result = parser.results[0]({
  variable: (d) => d,
  abstraction: (name, body) => ({ name, body }),
  application: (left, right) => ({ left, right })
})

console.log(result)
