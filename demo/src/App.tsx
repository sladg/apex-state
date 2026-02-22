import type { DeepKey } from '@sladg/apex-state'
import { useTransformedField } from '@sladg/apex-state'
import type { RowKey, SheetState } from './store'
import {
  BUDGET_CAP,
  Provider,
  concerns,
  initialState,
  sideEffects,
  useConcerns,
  useFieldStore,
  useSideEffects,
  useStore,
} from './store'

// ── Helpers ────────────────────────────────────────────────────────────

type SheetPath = DeepKey<SheetState>

const fmt = (n: number) =>
  n.toLocaleString('en-US', {
    style: 'currency',
    currency: 'USD',
    maximumFractionDigits: 0,
  })

// ── Registration Components ────────────────────────────────────────────

const Effects = () => {
  useSideEffects('budget-effects', sideEffects)
  return null
}

const FieldConcerns = () => {
  useConcerns('budget-concerns', concerns)
  return null
}

// ── Cell Components ────────────────────────────────────────────────────

const TextCell = ({ path }: { path: SheetPath }) => {
  const f = useFieldStore(path)
  const disabled = 'disabledWhen' in f ? (f.disabledWhen as boolean) : false
  const readonly = 'readonlyWhen' in f ? (f.readonlyWhen as boolean) : false
  const validation = 'validationState' in f
    ? (f.validationState as { isError: boolean; errors: { message: string }[] })
    : undefined
  const placeholder = 'dynamicPlaceholder' in f
    ? (f.dynamicPlaceholder as string)
    : ''
  const visible = 'visibleWhen' in f ? (f.visibleWhen as boolean) : true

  if (!visible) return <td className="cell col-hidden" />

  return (
    <td className={`cell ${validation?.isError ? 'cell-error' : ''}`}>
      <input
        type="text"
        value={String(f.value ?? '')}
        onChange={(e) => f.setValue(e.target.value)}
        disabled={disabled}
        readOnly={readonly}
        placeholder={placeholder}
        title={validation?.isError ? validation.errors[0]?.message : undefined}
      />
    </td>
  )
}

const NumberCell = ({ path }: { path: SheetPath }) => {
  const f = useFieldStore(path)
  const isReadonly = 'readonlyWhen' in f ? (f.readonlyWhen as boolean) : false
  const validation = 'validationState' in f
    ? (f.validationState as { isError: boolean; errors: { message: string }[] })
    : undefined
  const label = 'dynamicLabel' in f ? (f.dynamicLabel as string) : undefined

  return (
    <td
      className={`cell cell-number ${validation?.isError ? 'cell-error' : ''} ${isReadonly ? 'cell-readonly' : ''}`}
    >
      <input
        type="number"
        value={Number(f.value ?? 0)}
        onChange={(e) => f.setValue(Number(e.target.value))}
        readOnly={isReadonly}
        title={
          label ??
          (validation?.isError ? validation.errors[0]?.message : undefined)
        }
      />
    </td>
  )
}

const CurrencyCell = ({ path }: { path: SheetPath }) => {
  const f = useFieldStore(path)
  const isReadonly = 'readonlyWhen' in f ? (f.readonlyWhen as boolean) : false
  const validation = 'validationState' in f
    ? (f.validationState as { isError: boolean; errors: { message: string }[] })
    : undefined
  const label = 'dynamicLabel' in f ? (f.dynamicLabel as string) : undefined

  const transformed = useTransformedField(f as { value: number; setValue: (v: number) => void }, {
    to: (n: number) => n.toLocaleString('en-US'),
    from: (s: string) => Number(s.replace(/[^0-9.-]/g, '')) || 0,
  })

  return (
    <td
      className={`cell cell-currency ${validation?.isError ? 'cell-error' : ''} ${isReadonly ? 'cell-readonly' : ''}`}
    >
      <input
        type="text"
        value={transformed.value}
        onChange={(e) => transformed.setValue(e.target.value)}
        readOnly={isReadonly}
        title={
          label ??
          (validation?.isError ? validation.errors[0]?.message : undefined)
        }
      />
    </td>
  )
}

const ComputedCell = ({ path }: { path: SheetPath }) => {
  const [value] = useStore(path)
  return (
    <td className="cell cell-computed">
      <span>{fmt(value as number)}</span>
    </td>
  )
}

const CheckboxCell = ({ path }: { path: SheetPath }) => {
  const [value, setValue] = useStore(path)
  return (
    <td className="cell cell-checkbox">
      <input
        type="checkbox"
        checked={!!value}
        onChange={(e) => setValue(e.target.checked as never)}
      />
    </td>
  )
}

const ReadonlyCheckbox = ({ path }: { path: SheetPath }) => {
  const [value] = useStore(path)
  return (
    <td className="cell cell-checkbox cell-readonly">
      <input type="checkbox" checked={!!value} readOnly disabled />
    </td>
  )
}

const StatusCell = ({ path }: { path: SheetPath }) => {
  const f = useFieldStore(path)
  const visible = 'visibleWhen' in f ? (f.visibleWhen as boolean) : true
  if (!visible) return <td className="cell col-hidden" />

  const value = f.value
  const badge =
    value === 'High Budget'
      ? 'badge-high'
      : value === 'Medium'
        ? 'badge-med'
        : 'badge-low'
  return (
    <td className="cell cell-status">
      <span className={`badge ${badge}`}>{String(value)}</span>
    </td>
  )
}

const SelectCell = ({
  path,
  options,
}: {
  path: SheetPath
  options: string[]
}) => {
  const f = useFieldStore(path)
  const visible = 'visibleWhen' in f ? (f.visibleWhen as boolean) : true
  if (!visible) return <td className="cell col-hidden" />

  return (
    <td className="cell">
      <select
        value={String(f.value)}
        onChange={(e) => f.setValue(e.target.value)}
      >
        {options.map((o) => (
          <option key={o} value={o}>
            {o}
          </option>
        ))}
      </select>
    </td>
  )
}

const UtilizationCell = ({ row }: { row: RowKey }) => {
  const [pct] = useStore(`${row}.utilizationPct` as SheetPath)
  const value = pct as number
  const clamped = Math.min(value, 100)
  const barColor =
    value > 90 ? '#ef4444' : value > 70 ? '#f59e0b' : '#22c55e'

  return (
    <td className="cell cell-utilization">
      <div className="util-wrap">
        <span className="util-pct">{value.toFixed(1)}%</span>
        <div className="util-bar">
          <div
            className="util-fill"
            style={{ width: `${clamped}%`, background: barColor }}
          />
        </div>
      </div>
    </td>
  )
}

// ── Header Section ─────────────────────────────────────────────────────

const HeaderBar = () => {
  const [contactEmail, setContactEmail] = useStore('header.contactEmail')
  const [notificationEmail] = useStore('header.notificationEmail')
  const [showStatus, setShowStatus] = useStore('settings.showStatus')
  const [showPriority, setShowPriority] = useStore('settings.showPriority')
  const [showNotes, setShowNotes] = useStore('settings.showNotes')

  return (
    <div className="header-bar">
      <div className="header-field">
        <label>Contact Email</label>
        <input
          type="email"
          value={contactEmail}
          onChange={(e) => setContactEmail(e.target.value)}
        />
      </div>
      <div className="header-field">
        <label>
          Notification Email <span className="tag tag-sync">SYNC</span>
        </label>
        <input type="email" value={notificationEmail} readOnly />
      </div>
      <div className="header-field">
        <label>
          Columns <span className="tag tag-vis">VIS</span>
        </label>
        <div className="col-toggles">
          <label className="col-toggle">
            <input
              type="checkbox"
              checked={showStatus}
              onChange={(e) => setShowStatus(e.target.checked)}
            />
            Status
          </label>
          <label className="col-toggle">
            <input
              type="checkbox"
              checked={showPriority}
              onChange={(e) => setShowPriority(e.target.checked)}
            />
            Priority
          </label>
          <label className="col-toggle">
            <input
              type="checkbox"
              checked={showNotes}
              onChange={(e) => setShowNotes(e.target.checked)}
            />
            Notes
          </label>
        </div>
      </div>
    </div>
  )
}

// ── Data Row ───────────────────────────────────────────────────────────

const p = <R extends RowKey>(
  row: R,
  field: string,
): SheetPath => `${row}.${field}` as SheetPath

const DataRow = ({ row, idx }: { row: RowKey; idx: number }) => (
  <tr>
    <td className="cell cell-rownum">{idx}</td>
    {/* Col 1: Department — validationState */}
    <TextCell path={p(row, 'department')} />
    {/* Col 2: Project — dynamicPlaceholder */}
    <TextCell path={p(row, 'project')} />
    {/* Col 3–6: Q1–Q4 — currency formatted, validationState + readonlyWhen + dynamicLabel */}
    <CurrencyCell path={p(row, 'q1')} />
    <CurrencyCell path={p(row, 'q2')} />
    <CurrencyCell path={p(row, 'q3')} />
    <CurrencyCell path={p(row, 'q4')} />
    {/* Col 7: Annual Total — listener-computed, editable */}
    <CurrencyCell path={p(row, 'annualTotal')} />
    {/* Col 8: Util % — getter-computed */}
    <UtilizationCell row={row} />
    {/* Col 9: Quarterly Avg — AVG computation */}
    <ComputedCell path={p(row, 'quarterlyAvg')} />
    {/* Col 10: Approved — flip source */}
    <CheckboxCell path={p(row, 'approved')} />
    {/* Col 11: Rejected — flip target (auto-inverted) */}
    <ReadonlyCheckbox path={p(row, 'rejected')} />
    {/* Col 12: Selected — aggregation source */}
    <CheckboxCell path={p(row, 'selected')} />
    {/* Col 13: Status — set by listener */}
    <StatusCell path={p(row, 'status')} />
    {/* Col 14: Priority — dropdown select */}
    <SelectCell
      path={p(row, 'priority')}
      options={['Low', 'Medium', 'High', 'Critical']}
    />
    {/* Col 15: Notes — disabledWhen locked, visibleWhen detailed */}
    <TextCell path={p(row, 'notes')} />
    {/* Col 16: Locked — controls disabledWhen */}
    <CheckboxCell path={p(row, 'locked')} />
  </tr>
)

// ── Summary Row ────────────────────────────────────────────────────────

const SummaryRow = () => {
  const [grandTotal] = useStore('summary.grandTotal')
  const [allSelected, setAllSelected] = useStore('summary.allSelected')
  const [showStatus] = useStore('settings.showStatus')
  const [showPriority] = useStore('settings.showPriority')
  const [showNotes] = useStore('settings.showNotes')

  return (
    <tr className="summary-row">
      <td className="cell cell-rownum" />
      <td className="cell cell-summary" colSpan={2}>
        Summary
      </td>
      <td className="cell" colSpan={4} />
      <td className="cell cell-computed cell-grand">
        <span>{fmt(grandTotal)}</span>
      </td>
      <td className="cell" />
      <td className="cell" />
      <td className="cell" colSpan={2} />
      <td className="cell cell-checkbox">
        <input
          type="checkbox"
          checked={allSelected === true}
          onChange={(e) => setAllSelected(e.target.checked)}
        />
        <span className="tag tag-agg">AGG</span>
      </td>
      <td className={`cell ${!showStatus ? 'col-hidden' : ''}`} />
      <td className={`cell ${!showPriority ? 'col-hidden' : ''}`} />
      <td className={`cell ${!showNotes ? 'col-hidden' : ''}`} />
      <td className="cell" />
    </tr>
  )
}

// ── Column Headers ─────────────────────────────────────────────────────

const ColumnHeaders = () => {
  const [showStatus] = useStore('settings.showStatus')
  const [showPriority] = useStore('settings.showPriority')
  const [showNotes] = useStore('settings.showNotes')
  return (
    <thead>
      <tr>
        <th className="th">#</th>
        <th className="th">
          Department <span className="tag tag-val">VAL</span>
        </th>
        <th className="th">Project</th>
        <th className="th">Q1</th>
        <th className="th">Q2</th>
        <th className="th">Q3</th>
        <th className="th">Q4</th>
        <th className="th">
          Annual Total <span className="tag tag-listen">LSN</span>
        </th>
        <th className="th">
          Util % <span className="tag tag-get">GET</span>
        </th>
        <th className="th">
          Qtr Avg <span className="tag tag-avg">AVG</span>
        </th>
        <th className="th">Approved</th>
        <th className="th">
          Rejected <span className="tag tag-flip">FLIP</span>
        </th>
        <th className="th">
          Selected <span className="tag tag-agg">AGG</span>
        </th>
        <th className={`th ${!showStatus ? 'col-hidden' : ''}`}>
          Status <span className="tag tag-listen">LSN</span>
        </th>
        <th className={`th ${!showPriority ? 'col-hidden' : ''}`}>
          Priority
        </th>
        <th className={`th ${!showNotes ? 'col-hidden' : ''}`}>
          Notes <span className="tag tag-vis">VIS</span>
        </th>
        <th className="th">Locked</th>
      </tr>
    </thead>
  )
}

// ── Feature Legend ─────────────────────────────────────────────────────

const Legend = () => (
  <div className="legend">
    <h3>Features Demonstrated</h3>
    <div className="legend-grid">
      <div>
        <span className="tag tag-sync">SYNC</span> syncPaths — Contact
        &harr; Notification email
      </div>
      <div>
        <span className="tag tag-flip">FLIP</span> flipPaths — Approved
        &harr; Rejected (inverse booleans)
      </div>
      <div>
        <span className="tag tag-agg">AGG</span> aggregations — All
        Selected = consensus (Row 3 excluded when locked)
      </div>
      <div>
        <span className="tag tag-sum">SUM</span> computations — Grand
        Total = sum of all Q values
      </div>
      <div>
        <span className="tag tag-avg">AVG</span> computations — Quarterly
        Avg = average of Q1-Q4
      </div>
      <div>
        <span className="tag tag-listen">LSN</span> listeners — Annual
        Total auto-sums Q1-Q4 (editable); Status auto-set from total
      </div>
      <div>
        <span className="tag tag-get">GET</span> getter — Util % =
        annualTotal / {BUDGET_CAP.toLocaleString()} (reactive via computed())
      </div>
      <div>
        <span className="tag tag-val">VAL</span> validationState —
        Department (required), Q1-Q4 (max 100k per quarter)
      </div>
      <div>
        <span className="tag tag-vis">VIS</span> visibleWhen — Toggle
        Status, Priority, Notes columns via checkboxes
      </div>
      <div>
        <strong>disabledWhen</strong> — Notes input disabled when row is
        Locked
      </div>
      <div>
        <strong>readonlyWhen</strong> — Q1-Q4 readonly when row is Approved
      </div>
      <div>
        <strong>dynamicLabel</strong> — Hover Q1-Q4 to see &ldquo;Q1
        (Department)&rdquo;
      </div>
      <div>
        <strong>dynamicPlaceholder</strong> — Project field placeholder
        includes department name
      </div>
      <div>
        <strong>clearPaths</strong> — Changing Department clears Notes
      </div>
    </div>
  </div>
)

// ── Main App ───────────────────────────────────────────────────────────

const App = () => (
  <Provider initialState={initialState}>
    <Effects />
    <FieldConcerns />
    <div className="app">
      <h1>Budget Planning Sheet</h1>
      <p className="subtitle">
        50+ fields &middot; 16 columns &middot; 14 apex-state features
      </p>
      <HeaderBar />
      <div className="table-wrap">
        <table className="spreadsheet">
          <ColumnHeaders />
          <tbody>
            <DataRow row="row1" idx={1} />
            <DataRow row="row2" idx={2} />
            <DataRow row="row3" idx={3} />
          </tbody>
          <tfoot>
            <SummaryRow />
          </tfoot>
        </table>
      </div>
      <Legend />
    </div>
  </Provider>
)

export default App
