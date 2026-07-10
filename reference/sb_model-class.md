# Define the `sb_model` S4 Class

This class represents a (re)fitted sb model. It encapsulates the
algorithm used, hyperparameters, custom objective, feature data, target
variable, and the fitted model object.

## Slots

- `sb_algorithm`:

  A character string specifying the algorithm used ("ols", "glmnet",
  "rf", "xgb", "nn", "ew", "sw", "rp", "hrp", "mvo", "mmaf" or
  "custom_weights").

- `best_hyperparameters`:

  The chosen hyperparameters relevant to the specified machine learning
  algorithm. Applicable only for machine-learning algorithms.

- `model`:

  The fitted model object, which varies based on the algorithm used.

- `model_class`:

  A character string specifying the class of the model object.

- `eligible_signals`:

  A vector of eligible signals used to fit the model.

- `custom_objective`:

  A custom objective function used to fit the model.

- `huber_delta`:

  A numeric value specifying the delta parameter for the Huber loss
  function. Applicable only for machine-learning algorithms.

- `keras_architecture_parameters`:

  A list of parameters used to define the architecture of a neural
  network model. Applicable only for the "nn" algorithm.

## Methods

- `predict(new_features_m_df)`:

  Generates predictions using the fitted model on new feature data.
