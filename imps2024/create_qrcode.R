library(qrcode)

# set.seed() value omitted (its my birthday)
# get always the same image. good in case i need to change the url in the future

postercode <- qr_code("https://github.com/consistentlyBetter/ivd", "M")

generate_svg(
  postercode,
  "img/qrcode.svg",
  size = 300,
  foreground = "white",
  background = "#022851"
)
