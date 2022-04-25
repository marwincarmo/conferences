library(qrcode)

# set.seed() value omitted (its my birthday)
# get always the same image. good in case i need to change the url in the future

postercode <- qr_code("https://marwincarmo.github.io/posters/poster_viicp_spaq", "L")

generate_svg(
  postercode,
  "vii-clinica-psiquiatrica/spaq-network/img/qrcodeL.svg",
  size = 300,
  foreground = "white",
  background = "#024F84"
)
