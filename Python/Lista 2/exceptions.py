class Error(Exception):
  """Base class for other exceptions."""
  pass

class DivideByZero(Error):
  """Raised when user tries to divide by zero."""
  pass

class ValueIsNotAssigned(Error):
  """Raised when variable has no value assigned."""
  pass

class VariableDoesNotExist(Error):
  """Raised when variable does not exist."""
  pass 

class ValueIsNaN(Error):
  """Raised when value is not a number."""
  pass

