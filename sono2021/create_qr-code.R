library(qrcode)

# set.seed() value omitted (its my birthday)
# get always the same image. good in case i need to change the url in the future

postercode <- qr_code("https://marwincarmo.github.io/posters/cb_sono_poster", "M")

generate_svg(
  postercode,
  "conferences/cbsono_21/poster/img/qrcodeL.svg",
  size = 300,
  foreground = "white",
  background = "#024F84"
)
