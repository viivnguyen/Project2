#Here's the function we want in our package
top10 <- function(data, x){
  # Find the 10 top based on x
  top10x <- count(data, {{x}}) %>%
    slice_max(n = 10, order_by = n) %>%
    select({{x}}) %>%
    pull()
  # Filter dataset to only the 10 top based on x
  return(filter(data, {{x}} %in% top10x))
}
