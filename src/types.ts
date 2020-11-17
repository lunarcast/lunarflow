/**
 * Ts version of a -> b
 */
export type Fn<From, To> = (v: From) => To

/**
 * Encoding of purescript either for usage inside typescript
 */
export type PurescriptEither<L, R, T> = {
  left: Fn<L, T>
  right: Fn<R, T>
}
