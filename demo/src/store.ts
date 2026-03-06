import type {
  AggregationPair,
  BoolLogic,
  ComputationPair,
  ConcernRegistrationMap,
  DeepKey,
  GenericMeta,
  SideEffects,
  ValidationSchema,
} from '@sladg/apex-state'
import { createGenericStore } from '@sladg/apex-state'

// ── Types ──────────────────────────────────────────────────────────────

export interface RowData {
  department: string
  project: string
  q1: number
  q2: number
  q3: number
  q4: number
  annualTotal: number
  quarterlyAvg: number
  readonly utilizationPct: number
  approved: boolean
  rejected: boolean
  selected: boolean
  status: string
  priority: string
  notes: string
  locked: boolean
}

export interface SheetState {
  header: {
    contactEmail: string
    notificationEmail: string
  }
  settings: {
    showNotes: boolean
    showPriority: boolean
    showStatus: boolean
  }
  row1: RowData
  row2: RowData
  row3: RowData
  summary: {
    allSelected: boolean
    grandTotal: number
  }
}

export type RowKey = 'row1' | 'row2' | 'row3'
export type RowField = keyof RowData
export type RowPath<R extends RowKey, F extends RowField> = `${R}.${F}`

// ── Validators ─────────────────────────────────────────────────────────

const QUARTERLY_CAP = 100_000

const quarterlyBudget: ValidationSchema<number> = {
  safeParse: (data) => {
    if (typeof data === 'number' && data >= 0 && data <= QUARTERLY_CAP)
      return { success: true, data }
    return {
      success: false,
      error: {
        errors: [
          {
            path: [],
            message: `Must be 0–${QUARTERLY_CAP.toLocaleString()}`,
          },
        ],
      },
    }
  },
}

const nonEmptyString: ValidationSchema<string> = {
  safeParse: (data) => {
    if (typeof data === 'string' && data.trim().length > 0)
      return { success: true, data }
    return {
      success: false,
      error: { errors: [{ path: [], message: 'Required' }] },
    }
  },
}

// ── Constants ─────────────────────────────────────────────────────────

export const BUDGET_CAP = 300_000

// ── Side Effects Helpers ───────────────────────────────────────────────

const budgetLabel = (total: number) =>
  total > 200000 ? 'High Budget' : total > 100000 ? 'Medium' : 'Low Budget'

type SheetAgg = AggregationPair<SheetState>
type SheetComp = ComputationPair<SheetState>
type SheetBool = BoolLogic<SheetState>
type SheetKey = DeepKey<SheetState>

// ── Side Effects ───────────────────────────────────────────────────────

export const sideEffects: SideEffects<SheetState, GenericMeta> = {
  // ① Sync — contact ↔ notification email (bidirectional)
  //         showNotes → showPriority (one-way: toggling notes also enables priority, not vice-versa)
  syncPaths: [
    ['header.contactEmail', 'header.notificationEmail'],
    ['settings.showNotes', 'settings.showPriority', { oneWay: '[0]->[1]' }],
  ],

  // ② Flip — approved ↔ rejected (inverse booleans)
  flipPaths: [
    ['row1.approved', 'row1.rejected'],
    ['row2.approved', 'row2.rejected'],
    ['row3.approved', 'row3.rejected'],
  ],

  // ③ Aggregation — allSelected = consensus of row*.selected
  //    Row 3 excluded when locked (excludeWhen demo)
  aggregations: [
    ['summary.allSelected', 'row1.selected'] satisfies SheetAgg,
    ['summary.allSelected', 'row2.selected'] satisfies SheetAgg,
    [
      'summary.allSelected',
      'row3.selected',
      { IS_EQUAL: ['row3.locked', true] } satisfies SheetBool,
    ] satisfies SheetAgg,
  ],

  // ④ Computations — AVG for quarterly averages, SUM for grand total
  computations: [
    // Row 1 quarterly average
    ['AVG', 'row1.quarterlyAvg', 'row1.q1'] satisfies SheetComp,
    ['AVG', 'row1.quarterlyAvg', 'row1.q2'] satisfies SheetComp,
    ['AVG', 'row1.quarterlyAvg', 'row1.q3'] satisfies SheetComp,
    ['AVG', 'row1.quarterlyAvg', 'row1.q4'] satisfies SheetComp,
    // Row 2 quarterly average
    ['AVG', 'row2.quarterlyAvg', 'row2.q1'] satisfies SheetComp,
    ['AVG', 'row2.quarterlyAvg', 'row2.q2'] satisfies SheetComp,
    ['AVG', 'row2.quarterlyAvg', 'row2.q3'] satisfies SheetComp,
    ['AVG', 'row2.quarterlyAvg', 'row2.q4'] satisfies SheetComp,
    // Row 3 quarterly average
    ['AVG', 'row3.quarterlyAvg', 'row3.q1'] satisfies SheetComp,
    ['AVG', 'row3.quarterlyAvg', 'row3.q2'] satisfies SheetComp,
    ['AVG', 'row3.quarterlyAvg', 'row3.q3'] satisfies SheetComp,
    ['AVG', 'row3.quarterlyAvg', 'row3.q4'] satisfies SheetComp,
    // Grand total = sum of all 12 quarter values (can't use annualTotal as source — it's a target)
    ['SUM', 'summary.grandTotal', 'row1.q1'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row1.q2'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row1.q3'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row1.q4'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row2.q1'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row2.q2'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row2.q3'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row2.q4'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row3.q1'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row3.q2'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row3.q3'] satisfies SheetComp,
    ['SUM', 'summary.grandTotal', 'row3.q4'] satisfies SheetComp,
  ],

  // ⑤ Clear paths — changing department clears notes
  clearPaths: [
    [['row1.department'], ['row1.notes']],
    [['row2.department'], ['row2.notes']],
    [['row3.department'], ['row3.notes']],
  ],

  // ⑥ Listeners — auto-sum annualTotal from Q1-Q4 (editable, not a SUM target)
  //    + auto-set status when annual total changes
  listeners: [
    // Annual total auto-sum (one per row, uses scope for scoped changes)
    {
      path: 'row1' satisfies SheetKey,
      scope: 'row1' satisfies SheetKey,
      fn: (changes, state) => {
        if (!changes.some((c) => /^q[1-4]$/.test(c[0] as string)))
          return undefined
        const row = state as RowData
        return [['row1.annualTotal', row.q1 + row.q2 + row.q3 + row.q4]]
      },
    },
    {
      path: 'row2' satisfies SheetKey,
      scope: 'row2' satisfies SheetKey,
      fn: (changes, state) => {
        if (!changes.some((c) => /^q[1-4]$/.test(c[0] as string)))
          return undefined
        const row = state as RowData
        return [['row2.annualTotal', row.q1 + row.q2 + row.q3 + row.q4]]
      },
    },
    {
      path: 'row3' satisfies SheetKey,
      scope: 'row3' satisfies SheetKey,
      fn: (changes, state) => {
        if (!changes.some((c) => /^q[1-4]$/.test(c[0] as string)))
          return undefined
        const row = state as RowData
        return [['row3.annualTotal', row.q1 + row.q2 + row.q3 + row.q4]]
      },
    },
    // Status auto-set from annualTotal
    {
      path: 'row1.annualTotal' satisfies SheetKey,
      scope: null,
      fn: (_changes, state) => [
        ['row1.status', budgetLabel(state.row1.annualTotal)],
      ],
    },
    {
      path: 'row2.annualTotal' satisfies SheetKey,
      scope: null,
      fn: (_changes, state) => [
        ['row2.status', budgetLabel(state.row2.annualTotal)],
      ],
    },
    {
      path: 'row3.annualTotal' satisfies SheetKey,
      scope: null,
      fn: (_changes, state) => [
        ['row3.status', budgetLabel(state.row3.annualTotal)],
      ],
    },
  ],
}

// ── Concern Registrations ──────────────────────────────────────────────

const boolEquals = (
  path: DeepKey<SheetState>,
  value: boolean | string,
): { boolLogic: SheetBool } => ({
  boolLogic: { IS_EQUAL: [path, value] as never },
})

const quarterConcern = (row: RowKey, q: string) => ({
  validationState: { schema: quarterlyBudget },
  readonlyWhen: boolEquals(`${row}.approved`, true),
  dynamicLabel: { template: `${q} ({{${row}.department}})` },
})

export const concerns: ConcernRegistrationMap<SheetState> = {
  // Row 1
  'row1.department': { validationState: { schema: nonEmptyString } },
  'row1.project': {
    dynamicPlaceholder: { template: 'Project for {{row1.department}}' },
    validationState: { schema: nonEmptyString },
  },
  'row1.q1': quarterConcern('row1', 'Q1'),
  'row1.q2': quarterConcern('row1', 'Q2'),
  'row1.q3': quarterConcern('row1', 'Q3'),
  'row1.q4': quarterConcern('row1', 'Q4'),
  'row1.status': { visibleWhen: boolEquals('settings.showStatus', true) },
  'row1.priority': { visibleWhen: boolEquals('settings.showPriority', true) },
  'row1.notes': {
    disabledWhen: boolEquals('row1.locked', true),
    visibleWhen: boolEquals('settings.showNotes', true),
  },
  // Row 2
  'row2.department': { validationState: { schema: nonEmptyString } },
  'row2.project': {
    dynamicPlaceholder: { template: 'Project for {{row2.department}}' },
    validationState: { schema: nonEmptyString },
  },
  'row2.q1': quarterConcern('row2', 'Q1'),
  'row2.q2': quarterConcern('row2', 'Q2'),
  'row2.q3': quarterConcern('row2', 'Q3'),
  'row2.q4': quarterConcern('row2', 'Q4'),
  'row2.status': { visibleWhen: boolEquals('settings.showStatus', true) },
  'row2.priority': { visibleWhen: boolEquals('settings.showPriority', true) },
  'row2.notes': {
    disabledWhen: boolEquals('row2.locked', true),
    visibleWhen: boolEquals('settings.showNotes', true),
  },
  // Row 3
  'row3.department': { validationState: { schema: nonEmptyString } },
  'row3.project': {
    dynamicPlaceholder: { template: 'Project for {{row3.department}}' },
    validationState: { schema: nonEmptyString },
  },
  'row3.q1': quarterConcern('row3', 'Q1'),
  'row3.q2': quarterConcern('row3', 'Q2'),
  'row3.q3': quarterConcern('row3', 'Q3'),
  'row3.q4': quarterConcern('row3', 'Q4'),
  'row3.status': { visibleWhen: boolEquals('settings.showStatus', true) },
  'row3.priority': { visibleWhen: boolEquals('settings.showPriority', true) },
  'row3.notes': {
    disabledWhen: boolEquals('row3.locked', true),
    visibleWhen: boolEquals('settings.showNotes', true),
  },
}

// ── Initial State ──────────────────────────────────────────────────────

const makeRow = (
  dept: string,
  project: string,
  q1: number,
  q2: number,
  q3: number,
  q4: number,
): RowData => {
  const total = q1 + q2 + q3 + q4
  return {
    department: dept,
    project,
    q1,
    q2,
    q3,
    q4,
    annualTotal: total,
    quarterlyAvg: total / 4,
    get utilizationPct() {
      return (this.annualTotal / BUDGET_CAP) * 100
    },
    approved: false,
    rejected: true,
    selected: false,
    status: budgetLabel(total),
    priority: 'Medium',
    notes: '',
    locked: false,
  }
}

export const initialState: SheetState = {
  header: {
    contactEmail: 'budget@acme.com',
    notificationEmail: 'budget@acme.com',
  },
  settings: { showNotes: true, showPriority: true, showStatus: true },
  row1: makeRow('Engineering', 'Cloud Migration', 50000, 60000, 45000, 55000),
  row2: makeRow('Marketing', 'Brand Refresh', 30000, 35000, 40000, 25000),
  row3: makeRow('Operations', 'Automation', 20000, 15000, 25000, 30000),
  summary: {
    allSelected: false,
    grandTotal: 210000 + 130000 + 90000,
  },
}

// ── Store ──────────────────────────────────────────────────────────────

export const {
  Provider,
  useStore,
  useFieldStore,
  useSideEffects,
  useConcerns,
} = createGenericStore<SheetState>({
  debug: {
    devtools: true,
    log: true,
    track: true
  }
})
