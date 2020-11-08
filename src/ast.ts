// TODO: make this include position data.
type TermInput<T> = {
  application: (left: T, right: T) => T
  abstraction: (name: string, body: T) => T
  variable: (name: string) => T
}

type Term<T> = (input: TermInput<T>) => T

export const call = <T>(left: Term<T>, right: Term<T>): Term<T> => (config) =>
  config.application(left(config), right(config))

export const abstract = <T>(name: string, body: Term<T>): Term<T> => (config) =>
  config.abstraction(name, body(config))

export const variable = <T>(name: string): Term<T> => (config) =>
  config.variable(name)

export const abstractMany = <T>(names: string[], body: Term<T>): Term<T> =>
  names.reduceRight((previous, name) => abstract(name, previous), body)
