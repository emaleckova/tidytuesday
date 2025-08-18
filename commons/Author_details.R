# These are variables for repeated use
# Credits: https://www.r-bloggers.com/2023/07/adding-social-media-icons-to-charts-with-ggplot2/ and
# https://albert-rapp.de/posts/ggplot2-tips/08_fonts_and_icons/08_fonts_and_icons

github_icon <- "&#xf09b"
github_user <- "emaleckova"

linkedin_icon <- "&#xf08c"
linkedin_user <- "evamaleckova"

font_add("fa-brands", "commons/fonts/Font Awesome 7 Brands-Regular-400.otf")

social_caption <- glue::glue(
  "<span style='font-family:\"fa-brands\"'>{github_icon};</span>
  <span style='color: #000000'>{github_user}  | </span> <span style='color: #0077B5; font-family:\"fa-brands\"'>{linkedin_icon};</span>
  <span style='color: #000000 '>{linkedin_user}</span>"
)

# enable showtext
showtext_auto()
