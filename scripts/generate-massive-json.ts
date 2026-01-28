/**
 * Generate a massive deeply nested JSON object with up to 25 levels of nesting
 * Includes various data types: objects, arrays, primitives
 */

interface GenerateOptions {
  maxDepth: number
  branchingFactor: number // Number of keys per object level
  arraySize: number // Size of arrays at each level
  includeArrays: boolean
}

function generateRandomString(length = 10): string {
  return Math.random()
    .toString(36)
    .substring(2, 2 + length)
}

function generatePrimitive(): string | number | boolean | null {
  const types = ['string', 'number', 'boolean', 'null']
  const type = types[Math.floor(Math.random() * types.length)]

  switch (type) {
    case 'string':
      return generateRandomString()
    case 'number':
      return Math.floor(Math.random() * 1000)
    case 'boolean':
      return Math.random() > 0.5
    case 'null':
      return null
    default:
      return 'default'
  }
}

function generateMassiveObject(
  currentDepth: number,
  options: GenerateOptions,
): any {
  if (currentDepth >= options.maxDepth) {
    return generatePrimitive()
  }

  const shouldUseArray = options.includeArrays && Math.random() > 0.7

  if (shouldUseArray) {
    // Generate array
    return Array.from({ length: options.arraySize }, (_, index) => ({
      id: index,
      name: `item-${index}-depth-${currentDepth}`,
      value:
        currentDepth === options.maxDepth - 1
          ? generatePrimitive()
          : generateMassiveObject(currentDepth + 1, options),
    }))
  }

  // Generate object
  const obj: Record<string, any> = {}

  for (let i = 0; i < options.branchingFactor; i++) {
    const key = `level${currentDepth}_branch${i}`

    if (currentDepth === options.maxDepth - 1) {
      obj[key] = generatePrimitive()
    } else {
      obj[key] = generateMassiveObject(currentDepth + 1, options)
    }
  }

  return obj
}

export function createMassiveNestedObject(maxDepth = 25): any {
  const options: GenerateOptions = {
    maxDepth,
    branchingFactor: 3, // 3 keys per level = 3^25 theoretical leaf nodes
    arraySize: 3,
    includeArrays: true,
  }

  return {
    metadata: {
      generated: new Date().toISOString(),
      maxDepth,
      description: 'Massive deeply nested object for testing Valtio',
    },
    data: generateMassiveObject(0, options),
  }
}

// Generate and save to file if run directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const fs = require('fs')
  const path = require('path')

  const massive = createMassiveNestedObject(25)
  const outputPath = path.join(__dirname, '../fixtures/massive-nested.json')

  fs.mkdirSync(path.dirname(outputPath), { recursive: true })
  fs.writeFileSync(outputPath, JSON.stringify(massive, null, 2))

  console.log(`Generated massive nested object: ${outputPath}`)
  console.log(
    `File size: ${(JSON.stringify(massive).length / 1024).toFixed(2)} KB`,
  )
}
