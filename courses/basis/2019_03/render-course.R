createCourse(
  course      = "basis"
  , file.name   = "basis_descriptive"
  , title         = "R Basiskurs"
  , yaml.subtitle ="Introduction to R"
  , author      = "Stefan Coors & Patrick Schratz"
  , course.date = "27. März 2019"
  , subtitles   = names(intro_course)
  , year        = 2019
  , month       = 3
  , render      = TRUE
  , course.list = intro_course
  , template    = "preamble_reisensburg_dark.sty" # dark theme
  # , template    = "preamble_reisensburg.sty" # light theme
  , open.pdf    = FALSE
  # , custom.header = "pandoc_args: '--highlight=pygments'" # light theme
  , custom.header = "pandoc_args: '--highlight=../../../courses/_setup/breezedark.theme'" # dark theme
  , reset.exercise = FALSE
)
