type BinaryAst = {
  size: number
  bits: number[]
}

/**
 * Stores a binary ast into a buffer.
 *
 * @param ast The ast to create the buffer from.
 */
export function toBuffer({ size, bits }: BinaryAst) {
  const byteSize = Math.ceil(size / 8)
  const buffer = new ArrayBuffer(byteSize)

  const view = new Uint8Array(buffer)

  for (let i = 0; i < byteSize; i++) {
    const offset = i * 8
    const byteContent = Array(8)
      .fill(1)
      .map((_, index) => bits[offset + index] ?? 0)

    view[i] = byteContent.reduce(
      (previous, current) => (previous << 1) | current
    )
  }

  return buffer
}
