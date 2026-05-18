# soil_attr_balance

`soil_attr_balance()` is a generic R function designed to evaluate completeness and attribute-combination trade-offs in soil databases.

It helps identify which combinations of soil attributes preserve an acceptable number of complete observations while retaining as much analytical information as possible.

The function is especially useful for soil databases compiled from multiple sources, where different attributes may have very different levels of analytical coverage.

## 1. Purpose

Soil databases often contain many attributes, but not all attributes are available for the same samples, layers, profiles, or coordinates.

For example, a database may contain many observations with bulk density, texture, or pH, but fewer observations with carbon, phosphorus, exchangeable bases, or more specific chemical attributes.

This creates a practical problem when building datasets for:

1. Digital soil mapping.
2. Pedotransfer functions.
3. Soil carbon modeling.
4. Spectroscopic modeling.
5. Harmonized soil databases.
6. Exploratory multivariate analysis.
7. Model training datasets.

The central question answered by `soil_attr_balance()` is:

```text
Which combinations of soil attributes preserve enough complete observations while retaining a useful set of analytical variables?
