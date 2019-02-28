# forge 0.2.0

- All function gained an `id` parameter for the developer to specify a name for the input. This defaults to the developer inputed expression for `x`. The `return_id` parameter can be set when using the `%>%` operator to pass the ID to the next forge function.
- Added `certify()` along with helper functions e.g. `ge()`, `gte()`, `lt()`, for common comparisons. One can also provide arbitrary conditions in the form of functions.
- Revised `README` to reflect changes.
