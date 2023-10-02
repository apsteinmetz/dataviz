# unicode character bar chart

intToUtf8_v <- Vectorize(intToUtf8)

bar_chars <- data.frame(
  amt = (8:1) / 8,
  bar_char = trimws(intToUtf8_v(1:8 + 0x2587))
)

round_to_frac <- function(value, frac) {
  round(value * frac) / frac
}

build_bar <- Vectorize(function(value) {
  whole_part <- paste(rep("\U2588", times = floor(value)),
    collapse = ""
  )
  frac_amt <- round_to_frac(value - floor(value), 8)
  eighth_part <-
    bar_chars[bar_chars$amt == frac_amt, "bar_char"]
  return(trimws(paste0(whole_part, eighth_part)))
})

values <- runif(n = 5, max = 10)
bars <- build_bar(values)
print(data.frame(values, bars))
cbind(values, bars)
dplyr::tibble(values, bars)
