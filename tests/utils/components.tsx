import React from 'react'

/**
 * Universal ProductComponent for concerns-ui integration tests
 */
export const ProductComponent = ({
  store,
}: {
  store: {
    useFieldStore: (path: any) => any
    withConcerns: (selection: any) => { useFieldStore: (path: any) => any }
  }
}) => {
  const typeField = store.useFieldStore('type')
  const weightField = store
    .withConcerns({ visibleWhen: true })
    .useFieldStore('weight')
  const downloadUrlField = store
    .withConcerns({ visibleWhen: true })
    .useFieldStore('downloadUrl')
  const priceField = store
    .withConcerns({ disabledWhen: true })
    .useFieldStore('price')
  const isPublishedField = store.useFieldStore('isPublished')
  const nameField = store
    .withConcerns({ readonlyWhen: true })
    .useFieldStore('name')

  const showWeight = weightField.visibleWhen !== false
  const showDownloadUrl = downloadUrlField.visibleWhen !== false
  const priceDisabled = priceField.disabledWhen === true
  const nameReadOnly = nameField.readonlyWhen === true

  // Dynamic label based on type
  const labelText =
    typeField.value === 'digital' ? 'Software Name' : 'Product Name'

  // Dynamic placeholder
  const placeholderText =
    typeField.value === 'digital'
      ? 'https://download.example.com/software.zip'
      : 'Not applicable for physical products'

  // Dynamic tooltip
  const tooltipText =
    typeField.value === 'digital'
      ? 'Software products have flat pricing'
      : 'Physical products include shipping costs'

  return (
    <div>
      <select
        data-testid="type-select"
        value={typeField.value}
        onChange={(e: any) => typeField.setValue(e.target.value)}
      >
        <option value="physical">Physical</option>
        <option value="digital">Digital</option>
      </select>

      <label data-testid="name-label">{labelText}</label>
      <input
        data-testid="name-input"
        value={nameField.value}
        onChange={(e) => nameField.setValue(e.target.value)}
        readOnly={nameReadOnly}
        placeholder="Product Name"
      />

      <label>
        <input
          data-testid="published-checkbox"
          type="checkbox"
          checked={isPublishedField.value}
          onChange={(e) => isPublishedField.setValue(e.target.checked)}
        />
        Published
      </label>

      {showWeight && (
        <input
          data-testid="weight-input"
          type="number"
          value={weightField.value || ''}
          onChange={(e) =>
            weightField.setValue(
              e.target.value ? parseFloat(e.target.value) : undefined,
            )
          }
          placeholder="Weight (kg)"
        />
      )}

      {showDownloadUrl && (
        <input
          data-testid="download-url-input"
          value={downloadUrlField.value || ''}
          onChange={(e) => downloadUrlField.setValue(e.target.value)}
          placeholder={placeholderText}
        />
      )}

      <div data-testid="price-tooltip" title={tooltipText}>
        <input
          data-testid="price-input"
          type="number"
          value={priceField.value}
          onChange={(e) => priceField.setValue(parseFloat(e.target.value))}
          disabled={priceDisabled}
          placeholder="Price"
        />
      </div>
      {priceDisabled && (
        <span data-testid="price-disabled-msg">Price is locked</span>
      )}
    </div>
  )
}

/**
 * Universal CartComponent for aggregations integration tests
 */
export const CartComponent = ({
  store,
}: {
  store: {
    useFieldStore: (path: any) => any
    useJitStore: () => any
  }
}) => {
  const { getState, setChanges } = store.useJitStore()
  const items = store.useFieldStore('items')
  const subtotal = store.useFieldStore('subtotal')
  const tax = store.useFieldStore('tax')
  const total = store.useFieldStore('total')
  const itemCount = store.useFieldStore('itemCount')

  const handleAddItem = () => {
    const state = getState()
    const newId = `item-${Date.now()}`
    const newItem = {
      name: 'New Item',
      price: 15,
      quantity: 1,
      subtotal: 15,
    }
    const newSubtotal = state.subtotal + 15
    const newCount = Object.keys(state.items).length + 1
    setChanges([
      [`items.${newId}`, newItem, {}],
      ['subtotal', newSubtotal, {}],
      ['itemCount', newCount, {}],
    ])
  }

  const handleRemoveItem = (itemId: string) => {
    const state = getState()
    const item = state.items[itemId]
    if (!item) return
    const newSubtotal = state.subtotal - item.subtotal
    setChanges([
      [`items.${itemId}`, undefined, {}],
      ['subtotal', newSubtotal, {}],
    ])
  }

  const handleChangeQuantity = (itemId: string, newQuantity: number) => {
    const state = getState()
    const item = state.items[itemId]
    if (!item) return
    const newSubtotal = item.price * newQuantity
    setChanges([
      [`items.${itemId}.quantity`, newQuantity, {}],
      [`items.${itemId}.subtotal`, newSubtotal, {}],
    ])
  }

  const handleChangePrice = (itemId: string, newPrice: number) => {
    const state = getState()
    const item = state.items[itemId]
    if (!item) return
    const newSubtotal = newPrice * item.quantity
    setChanges([
      [`items.${itemId}.price`, newPrice, {}],
      [`items.${itemId}.subtotal`, newSubtotal, {}],
    ])
  }

  const handleUpdateSubtotal = (newSubtotal: number) => {
    const newTax = newSubtotal * 0.1
    setChanges([
      ['subtotal', newSubtotal, {}],
      ['tax', newTax, {}],
    ])
  }

  const handleUpdateValues = (subtotalVal: number) => {
    const taxVal = subtotalVal * 0.1
    const totalVal = subtotalVal + taxVal
    setChanges([
      ['subtotal', subtotalVal, {}],
      ['tax', taxVal, {}],
      ['total', totalVal, {}],
    ])
  }

  return (
    <div>
      <button data-testid="add-item-btn" onClick={handleAddItem}>
        Add Item
      </button>
      <button data-testid="add-btn" onClick={handleAddItem}>
        Add
      </button>
      <button
        data-testid="remove-btn"
        onClick={() => handleRemoveItem('item-1')}
      >
        Remove
      </button>
      <button
        data-testid="change-qty-btn"
        onClick={() => handleChangeQuantity('item-1', 5)}
      >
        Set Qty to 5
      </button>
      <button
        data-testid="change-price-btn"
        onClick={() => handleChangePrice('item-1', 50)}
      >
        Set Price to 50
      </button>
      <button
        data-testid="update-subtotal-btn"
        onClick={() => handleUpdateSubtotal(100)}
      >
        Set Subtotal to 100
      </button>
      <button data-testid="update-btn" onClick={() => handleUpdateValues(200)}>
        Update
      </button>

      <span data-testid="item-count">{itemCount.value}</span>
      <span data-testid="count">{itemCount.value}</span>
      <span data-testid="subtotal">{subtotal.value}</span>
      <span data-testid="tax">{tax.value}</span>
      <span data-testid="total">{total.value}</span>

      {/* Legacy support if needed */}
      <span data-testid="item-count-legacy">
        {Object.keys(items.value).length}
      </span>
    </div>
  )
}

/**
 * Universal WizardComponent for complex-workflows integration tests
 */
export const WizardComponent = ({
  store,
  delay = 0,
}: {
  store: {
    useFieldStore: (path: any) => any
    useJitStore: () => any
  }
  delay?: number
}) => {
  const { getState } = store.useJitStore()
  const currentStepField = store.useFieldStore('currentStep')
  const personalInfoField = store.useFieldStore('personalInfo')
  const addressInfoField = store.useFieldStore('addressInfo')
  const errorsField = store.useFieldStore('_errors')

  const [isValidating, setIsValidating] = React.useState(false)

  const validateStep = (step: number): boolean => {
    const errors = { ...errorsField.value }
    let isValid = true

    const state = getState()
    if (step === 1) {
      const { firstName, lastName } = state.personalInfo
      if (!firstName) {
        errors['personalInfo.firstName'] = ['First name required']
        isValid = false
      } else {
        delete errors['personalInfo.firstName']
      }
      if (!lastName) {
        errors['personalInfo.lastName'] = ['Last name required']
        isValid = false
      } else {
        delete errors['personalInfo.lastName']
      }
    }

    errorsField.setValue(errors)
    return isValid
  }

  const handleNext = async () => {
    const currentStep = getState().currentStep
    if (validateStep(currentStep)) {
      setIsValidating(true)
      if (delay > 0) {
        await new Promise((resolve) => setTimeout(resolve, delay))
      } else {
        await Promise.resolve()
      }
      currentStepField.setValue(currentStep + 1)
      setIsValidating(false)
    }
  }

  const handlePrev = () => {
    if (currentStepField.value > 1) {
      currentStepField.setValue(currentStepField.value - 1)
    }
  }

  const aggregateData = () => {
    const state = getState()
    const { firstName, lastName } = state.personalInfo
    const { street, city, zipCode } = state.addressInfo
    return `${firstName} ${lastName}, ${street}, ${city} ${zipCode}`
  }

  const steps = ['Personal Info', 'Address Info', 'Review']

  return (
    <div>
      <span data-testid="current-step">{currentStepField.value}</span>

      <div data-testid="progress">
        {steps.map((label: string, idx: number) => (
          <div
            key={idx}
            data-testid={`step-${idx + 1}`}
            className={currentStepField.value === idx + 1 ? 'active' : ''}
          >
            {label}
          </div>
        ))}
      </div>
      <span data-testid="progress-text">
        Step {currentStepField.value} of {steps.length}
      </span>

      {currentStepField.value === 1 && (
        <div data-testid="step1-form">
          <input
            data-testid="firstName-input"
            value={personalInfoField.value.firstName}
            onChange={(e) => {
              const current = getState().personalInfo
              personalInfoField.setValue({
                ...current,
                firstName: e.target.value,
              })
            }}
            placeholder="First Name"
          />
          <input
            data-testid="lastName-input"
            value={personalInfoField.value.lastName}
            onChange={(e) => {
              const current = getState().personalInfo
              personalInfoField.setValue({
                ...current,
                lastName: e.target.value,
              })
            }}
            placeholder="Last Name"
          />
        </div>
      )}

      {currentStepField.value === 2 && (
        <div data-testid="step2-form">
          <input
            data-testid="street-input"
            value={addressInfoField.value.street}
            onChange={(e) => {
              const current = getState().addressInfo
              addressInfoField.setValue({
                ...current,
                street: e.target.value,
              })
            }}
            placeholder="Street"
          />
          <input
            data-testid="city-input"
            value={addressInfoField.value.city}
            onChange={(e) => {
              const current = getState().addressInfo
              addressInfoField.setValue({
                ...current,
                city: e.target.value,
              })
            }}
            placeholder="City"
          />
        </div>
      )}

      {currentStepField.value === 3 && (
        <div data-testid="review-section">
          <h3>Review Your Information</h3>
          <p data-testid="aggregated-data">{aggregateData()}</p>
          <p data-testid="personal-summary">
            {personalInfoField.value.firstName}{' '}
            {personalInfoField.value.lastName}
          </p>
          <p data-testid="address-summary">
            {addressInfoField.value.street}, {addressInfoField.value.city}{' '}
            {addressInfoField.value.zipCode}
          </p>
        </div>
      )}

      <button
        data-testid="prev-btn"
        onClick={handlePrev}
        disabled={currentStepField.value === 1}
      >
        Back
      </button>

      {/* Supports both sync and async next buttons from tests */}
      <button
        data-testid="next-btn"
        onClick={handleNext}
        disabled={isValidating}
      >
        {isValidating ? 'Validating...' : 'Next'}
      </button>

      {/* Button for TC6.3 specifically if needed, but next-btn above should work */}

      {errorsField.value['personalInfo.firstName'] && (
        <span data-testid="firstName-error">
          {errorsField.value['personalInfo.firstName'][0]}
        </span>
      )}

      {errorsField.value['email'] && (
        <span data-testid="error-message">{errorsField.value['email'][0]}</span>
      )}
      <span data-testid="error-count">
        {Object.keys(errorsField.value).length}
      </span>
    </div>
  )
}

/**
 * Generic Component for basic integration tests
 */
export const Component = ({
  store,
  sideEffects,
}: {
  store: {
    useJitStore: () => any
    useSideEffects: (id: string, effects: any) => void
  }
  sideEffects?: any
}) => {
  if (sideEffects) {
    store.useSideEffects('test', sideEffects)
  }

  const { proxyValue } = store.useJitStore()

  return (
    <div>
      {Object.keys(proxyValue).map((key) => (
        <span key={key} data-testid={key}>
          {typeof proxyValue[key] === 'boolean'
            ? proxyValue[key].toString()
            : proxyValue[key]?.toString()}
        </span>
      ))}
      <span data-testid="field-value">{proxyValue.value}</span>
      <span data-testid="proxy-value">{proxyValue.value}</span>
    </div>
  )
}
