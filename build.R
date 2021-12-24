library(distill)
library(rmarkdown)

create_blog(dir = "nickzani", title = "My Blog")
render_site()

create_post("Advent of Code: Day 1")
