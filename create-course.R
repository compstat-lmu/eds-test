## Main function:
## --------------

#' @title This is the main function which creates the final course.
#'
#' @description
#'   With \code{createCourse} one can create a course based on a course list. To
#'   see how those lists have to look like see the How to use in the Readme.
#'
#'   **NOTE:** This function is implemented very uniffecient with this helper
#'   function 'n()'. But this helps reading the code and even for larger courses
#'   the performance is sufficient.
#'
#' @param title [\code{character(1)}]\cr
#'   Title of the course. If this is setted to \code{NA}, than the title must be
#'   specified via the console.
#' @param course [\code{character(1)}]\cr
#'   This is the directiory name for the course (e. g. \code{basis}).
#' @param file.name [\code{character(1)}]\cr
#'   Name for the \code{Rmd} file.
#' @param subtitles [\code{character}]\cr
#'   Subtitle of the chapters. The length have to match the length of the
#'   \code{course.list} object.
#' @param course.list [\code{list}]\cr
#'   The list object containing the course structure.
#' @param path [\code{character(1)}]\cr
#'   Path to the r coruses repo.
#' @param render [\code{logical(1)}]\cr
#'   If the created file should be rendered after creation, than set this
#'   parameter to TRUE (default).
#' @param suppress [\code{logical(1)}]\cr
#'   If \code{TRUE} than the messages of the rendering will be suppressed.
#' @param year [\code{character(1)}]\cr
#'   Year of the course (if \code{NA}, than the year needs be specified via the
#'   console).
#' @param month [\code{character(1)}]\cr
#'   Month of the course (if \code{NA}, than the month needs be specified via the
#'   console).
#' @param author [\code{character(1)}]\cr
#'   Author of the course (if \code{NA}, than the author needs be specified via the
#'   console).
#' @param course.date [\code{character(1)}]\cr
#'   When did the course take place (if \code{NA}, than the duration needs be
#'   specified via the console).
#' @param bibliography [\code{character(1)}]\cr
#'   Specify a .bib object in the data folder.
#' @param reset.exercise [\code{logical(1)}]\cr
#'   Logical parameter which specifys if the exercise counter should be
#'   resetted after every subsection.
#' @param yaml.subtitle {\code{character(1)}}\cr
#'   Set the subtitle in the yaml header. This will overrite the subtitle vector
#'   of length one which is also used in the yaml header (if it is the only
#'   subtitle for just one section).
#' @param open.pdf [\code{logical(1)}]\cr
#'   If the rendered pdf should be opened after the rendering, than set
#'   'open.pdf = TRUE'. This is only possible if 'render = TRUE'.
#' @param scope [\code{character(1)}] \cr
#'   Character naming the file in which the scope should be stored. If nothing
#'   is given, then no scope is created.
#' @param clean.dir [\code{logical(1)}]\cr
#'   Logical value indecating if the cache, files, knit-figures, and .tex files should be
#'   deleted or not.
#' @param custom.pandoc [\code{character(1)}]\cr
#'   Character string indicating if a custom pandoc should be used or not. If no
#'   custom pandoc should be used pass \code{custom.pandoc = NULL}.
#' @param xaringan [\code{logical(1)}]\cr
#'   Should 'xaringan::moon_reader' be the output format?
#' @param xaringan.pdf [\code{logical(1)}]\cr
#'   Whether to convert the html xaringan slides to PDF via 'decktape'
#' @param xelatex [\code{character(1)}]\cr
#'   Character string indicating if xelatex should be used for rendering instead of pdflatex.
#' @return [\code{character}]\cr
#'   A character vector containing the lines of the fineal Rmd file.
createCourse = function (title, course, file.name, subtitles = NA, course.list,
  path = getwd(), render = TRUE, suppress = FALSE, year, month,
  author = NA, course.date = NA, keep.source = FALSE,
  bibliography, reset.exercise = TRUE, yaml.subtitle = NA, open.pdf = TRUE,
  scope, clean.dir = TRUE, template = "preamble_munichre.sty", custom.pandoc = "custom_pandoc.tex",
  xaringan = FALSE, xaringan.pdf = FALSE, xelatex = FALSE, custom.header = "", ratio = "4:3")
{

  require(dplyr)
  # Get path to rcourses root:
  #path = paste0(strsplit(path, "/rcourses")[[1]][1], "/rcourses")

  # Check if latex math is available:
  # if (! dir.exists(paste0(path, "/latex-math"))) {
  #   warning("\"latex-math\" math is not available. Please clone the repository ",
  #     "into you rcourses directory with \"git clone https://github.com/compstat-lmu/latex-math.git\" ",
  #     "to avoid errors while running pdflatex. For additional instructions see:\n\n ",
  #     "\t\"https://github.com/compstat-lmu/latex-math\""
  #   )
  # }
  # Get available structure:
  #struct = getStructure()

  # Check if all slides are available:
  # missing.slides = NULL
  # for (i in seq_along(course.list)) {
  #   for (j in seq_along(course.list[[i]])) {
  #     # Slides are just characters, all other inputs in the course.list have
  #     # other classes:
  #     if (class(course.list[[i]][[j]]) == "character") {
  #       # Internal check in readSlide:
  #       e = try(readSlide(course.list[[i]][[j]]), silent = TRUE)
  #       if (class(e) == "try-error") {
  #         missing.slides = append(missing.slides, course.list[[i]][[j]])
  #       }
  #     }
  #   }
  # }
  # if (! is.null(missing.slides[1])) {
  #   stop("Follwoing slides are not present within the chapters:\n",
  #     paste(paste0("\t - slides_", missing.slides, ".Rmd"), collapse = "\n"), "\n")
  # }

  # Auxiliary function which returns the length of the actual lines of the
  # course object.
  n = function ()
  {
    length(course.text)
  }

  # Check if the path is setted correctly:
  # if (! dir.exists(paste0(path, "/courses"))) {
  #   stop("The path has to point to the working directory of the courses repo!")
  # }
  # Check if there are equally much subtitles given then list elements of the
  # course list:
  # if (length(subtitles) != length(course.list)) {
  #   stop("Specify the subtitles or set it to NA if you don't want to have subtitles!")
  # }
  # Create as many NA's as lists in 'course.list' to supress subtitles.
  if (length(subtitles) == 1 && is.na(subtitles[1])) {
    subtitles = rep(NA, length(course.list))
  }

  # Check if a year and month is give, if not type them into the console:
  if (missing(year)) {
    stop("Specify the \"year\" when the course takes place as int or character!")
  }
  if (missing(month)) {
    stop("Specify the \"month\" when the course takes place (convention is 03 for march)!")
  }

  # Unify the month string with a '0' in front of numbers smaller than 10:
  if (nchar(month) < 2) {
    month = paste0("0", month)
  }

  # Create the date for the directiory of the new course and the path to that
  # course:
  date = paste0(year, "_", month)
  path.to.course = paste0(path, "/courses/", course, "/", date)

  # Check if the directory already exists, if not, create it:
  if (dir.exists(path.to.course)) {
    cat("\nDate already exists, creating course into\n\n\t", path.to.course, "\n")
  } else {
    # The creation of the dir contains two steps:
    #   1. Check if the course exists, if not create it
    #   2. Check if the date exists, if not create it
    cat("\nThis is the first course at the given date. Create directiory:\n\n\t",
      path.to.course, "\n")
    if (! dir.exists(paste0(path, "/courses/", course))) {
      dir.create(paste0(path, "/courses/", course))
    }
    if (! dir.exists(path.to.course)) {
      dir.create(path.to.course)
    }
  }

  # if (! missing(scope)) {
  #   if (scope != FALSE) {
  #     file.scope = scope
  #     scope      = TRUE
  #   }
  # } else {
  #   scope = FALSE
  # }

  # if (scope) {
  #   scope.and.schedule = c(
  #     "## Scope of this course",
  #     "",
  #     "- First point",
  #     "- Second point",
  #     "",
  #     "## General Schedule Day 1",
  #     "",
  #     "| Time  | Content                                     |",
  #     "| ----- | ------------------------------------------- |",
  #     "| 09:00 | Start                                       |",
  #     "| 11:00 | Coffee Break                                |",
  #     "| 12:30 | **Break + Lunch**                           |",
  #     "| 15:00 | Coffee Break                                |",
  #     "| 16:30 | End                                         |",
  #     "",
  #     "## General Schedule Day 2",
  #     "",
  #     "| Time  | Content                                     |",
  #     "| ----- | ------------------------------------------- |",
  #     "| 09:00 | Start                                       |",
  #     "| 11:00 | Coffee Break                                |",
  #     "| 12:30 | **Break + Lunch**                           |",
  #     "| 15:00 | Coffee Break                                |",
  #     "| 16:30 | End                                         |",
  #     ""
  #   )
  # }
  #
  # # Write the scope and schedule file, if not existing!
  # if (scope) {
  #   if (! file.exists(paste0(path.to.course, "/", file.scope, ".Rmd"))) {
  #     writeLines(
  #       text = scope.and.schedule,
  #       con  = paste0(path.to.course, "/", file.scope, ".Rmd")
  #     )
  #   }
  # }

  # Empty string for the lines in the course, every element of `course.text` is
  # one line of the resulting Rmd file:
  course.text = character()

  # First set up the YAML header:
  course.text[1] = "---"

  # Check title:
  if (missing(title)) {
    stop("Specify the \"title\" of the course!")
  } else {
    course.text[n() + 1] = paste0("title: ", title)
  }

  # Check subtitle:
  if (! missing(yaml.subtitle)) {
      course.text[n() + 1] = paste0("subtitle: ", yaml.subtitle, "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>")
  }

  # Check author:
  if (is.na(author)) {
    cat("Please type in the author of the course:")
    author = readLines(n = 1L)
    cat("\n")
  }
  course.text[n() + 1] = paste0("author: ", author)

  # Check duration of the course:
  if (missing(course.date)) {
    stop("Specify the exact \"course.date\" of the course!")
  }
  course.text[n() + 1] = paste0("date: ", course.date)

 # if (xaringan) {
    custom.pandoc = NULL
    # Set up the basic header with the preamble and the style file:
    if (ratio == "16:9") {
      course.text[n() + 1] = "classoption: 'aspectratio=169'"
    }
    course.text[n() + 1] = 'output:'
    course.text[n() + 1] = '  xaringan::moon_reader:'
    course.text[n() + 1] = '    lib_dir: libs'
    course.text[n() + 1] = '    nature:'
    course.text[n() + 1] = '      before_init: ../../_setup/js/macros.js'
    course.text[n() + 1] = '      highlightStyle: googlecode'
    course.text[n() + 1] = '      highlightLines: true'
    course.text[n() + 1] = '      countIncrementalSlides: false'
    course.text[n() + 1] = '    css: [metropolis, metropolis-fonts, ../../_setup/css/xaringan-theme.css, ../../_setup/css/tweaks.css, ../../_setup/css/gentle-ggplot.css]'
  #}

  # If custom pandoc is defined:
  if (! is.null(custom.pandoc)) {
    course.text[n() + 1] = paste0('    template: "../../_setup/', custom.pandoc, '"')
  }

  # If somebody wants to include a bibliography (Here you can see the advantage
  # of the function 'n()'):
  if (! missing(bibliography)) {
    course.text[n() + 1] = paste0('bibliography: "../../../data/',
      bibliography, '"')
  }

    # Leftover of the header and some other stuff:
    course.text[n() + 1] = '---'
    course.text[n() + 1] = ''
    course.text[n() + 1] = '<!-- This file was created autmatically! Please check the entrys! -->'
    course.text[n() + 1] = ''
    course.text[n() + 1] = '```{r setup, include=FALSE}'
    course.text[n() + 1] = 'knitr::opts_chunk$set(fig.width=4.25, fig.height=3.5, fig.retina=3,
      out.width = "100%",
      message=FALSE, warning=FALSE, autodep = FALSE, hiline=TRUE)\n'
    course.text[n() + 1] = 'knitr::opts_hooks$set(fig.callout = function(options) {
      if (options$fig.callout) {
        options$echo <- FALSE
        options$out.height <- "99%"
        options$fig.width <- 16
        options$fig.height <- 8
      }
      options
    })'
    course.text[n() + 1] = '```'
    course.text[n() + 1] = ''

  # if (scope) {
  #   course.text[n() + 1] = ''
  #   course.text[n() + 1] = paste0('```{r, child = "', file.scope, '.Rmd",include=FALSE}')
  #   course.text[n() + 1] = '```'
  #   course.text[n() + 1] = ''
  # }

  ## Main loop to fill the course.text vector with the chapters and other
  ## slides:
  ##
  ## The main loop iters over the lists in the course.list object, hence over
  ## all the sections:


  for (i in seq_along(course.list)) {

    # If a subtitle for sections after the first one is given, create a new
    # slide and reset the exercises counter:
    if (! is.na(subtitles[i])) {

      if (reset.exercise && (i > 1)) {
        course.text[n() + 1] = "\\resetExercises"
        course.text[n() + 1] = ""
      }
      course.text[n() + 1] = "---"
      course.text[n() + 1] = ""
      course.text[n() + 1] = "class: inverse, center, middle"
      course.text[n() + 1] = ""
      course.text[n() + 1] = paste0("# ", subtitles[i])
      course.text[n() + 1] = ""
      course.text[n() + 1] = "---"
      course.text[n() + 1] = ""

    }

    # The second loop iters over the slides given in one section:
    for (j in seq_along(course.list[[i]])) {
      # Empty line:
      course.text[n() + 1] = ""

      # If the slide is named and exists, than read that slide with the
      # readSlide function:
      if (class(course.list[[i]][[j]]) == "character") {

        # Get all rows of the slides with the specific name and check if there
        # are name dispatches:
        #dir = struct %>% filter(files == course.list[[i]][[j]])

        # If there are more than one match, skip that file:
        # if (nrow(dir) > 1) {
        #   warning("Skip \"", course.list[[i]][[j]], "\", there are more than one matches for the slides!")
        # } else {

          if (keep.source) {
            source = readLines(con = readSlide(course.list[[i]][[j]]))

            # course.text[n() + 1] = "<!-- ----------------------------------------------------------------------- -->"
            # course.text[n() + 1] = paste0(
            #   "<!-- ",
            #   course.list[[i]][[j]], paste(rep(" ", 71 - nchar(course.list[[i]][[j]])), collapse = ""),
            #   " -->"
            # )
            # course.text[n() + 1] = "<!-- ----------------------------------------------------------------------- -->"
            course.text[n() + 1] = ""
            course.text[n() + 1] = ""
            course.text[n() + 1] = ""
            course.text[n() + 1] = ""
            course.text[n() + 1] = ""
            course.text[n() + 1] = ""

            # Manually set wd, otherwise the images can't be included:
            # course.text[n() + 1] = "```{r, include=FALSE}"
            # course.text[n() + 1] = paste0("getwd = function () { return (readSlide(\"", course.list[[i]][[j]],"\", file = FALSE)) }")
            # course.text[n() + 1] = "```"
            # course.text[n() + 1] = ""
            for (source.line in source) {

              # Remove ap and include directory of readSlide instead. This allows to read
              # figures, code, etc. to be loaded even if we are not in child mode:
              if (grepl(x = source.line, pattern = "adjust_path")) {
                # replace "adjust_path(paste0(getwd()" with (in the case of the roc slide)
                # "function (myfile) paste0(file.path(readSlide("roc", file = FALSE),"
                source.line = stringr::str_replace(
                  string      = source.line,
                  pattern     = "adjust_path\\(paste0\\(getwd\\(\\)",
                  replacement = paste0(
                    "function \\(myfile\\) paste0\\(file.path\\(readSlide(\"",
                    course.list[[i]][[j]],
                    "\", file = FALSE)"
                  )
                )
                # replace "))" with ", myfile))"
                source.line = stringr::str_replace(string = source.line, pattern = "\\)\\)",
                  replace = ", myfile\\)\\)")
              }

              course.text[n() + 1] = source.line
            }

            course.text[n() + 1] = ""
            course.text[n() + 1] = ""

          } else {
            # Check if file extists:
            temp = readSlide(course.list[[i]][[j]])

            # if(xaringan){
            #   course.text[n() + 1] = paste0(readr::read_file(temp))
            #
            # } else {

            # Read in the slides:
            course.text[n() + 1] = paste0('```{r, child=readSlide("', course.list[[i]][[j]], '")}')
            course.text[n() + 1] = "```"
            course.text[n() + 1] = ""
            #}
            rm(temp)
          }
        #}
      }

      # If the class of a slide is 'empty_slide' iter over the given lines and
      # write them into the course.text object:
      if (class(course.list[[i]][[j]]) == "empty_slide") {
        for (k in seq_along(course.list[[i]][[j]])) {
          course.text[n() + 1] = course.list[[i]][[j]][k]
        }
        course.text[n() + 1] = ""
      }

      # If the class of a slide is 'exercise_slide' iter over the lines created
      # of the 'nExercise' function:
      if (class(course.list[[i]][[j]]) == "exercise_slide") {
        for (k in seq_along(course.list[[i]][[j]])) {
          course.text[n() + 1] = course.list[[i]][[j]][k]
        }
        course.text[n() + 1] = ""
      }
    }
  }

  if (! missing(bibliography)) {
    course.text[n() + 1] = ""
    course.text[n() + 1] = "<!-- References -->"
    course.text[n() + 1] = "#"
    course.text[n() + 1] = "\\tiny"
    course.text[n() + 1] = ""
  }

  # If there is no specific name given for the Rmd file, name it like the course
  # and paste '_final' at the and:
  if (missing(file.name)) {
    file.name = paste0(course, "_final")
  }

  # Create path for the Rmd file:
  rmd.file = paste0(path.to.course, "/", file.name, ".Rmd")

  # Write the lines contained in course.text to the Rmd file:
  writeLines(text = course.text, con  = rmd.file)

  # If the Rmd file should be rendered, this happens here:
  if (render) {

    rmarkdown::render(input = rmd.file)


    # if (suppress) {
    #   if (xelatex) {
    #     BBmisc::suppressAll(rmarkdown::render(input = rmd.file, output_options = list(latex_engine = "xelatex")))
    #   } else {
    #     BBmisc::suppressAll(rmarkdown::render(input = rmd.file))
    #   }
    # } else {
    #   if (xelatex) {
    #     rmarkdown::render(input = rmd.file, output_options = list(latex_engine = "xelatex"))
    #   } else {
    #     rmarkdown::render(input = rmd.file)
    #   }
    # }

    # clean = TRUE in render does not work here???

    # Convert .html to .pdf using 'decktape' via 'docker'
    # if (xaringan.pdf) {
    #   print(path.to.course)
    #   system(glue::glue("docker run --rm -t -v `pwd`:/slides -v $HOME:$HOME astefanutti/decktape:2.8.0 {path.to.course}/{file.name}.html {path.to.course}/{file.name}.pdf"))
    # }

    # if (clean.dir) {
    #   # Remove cache, knit-figures folder and .tex file to avoid unexpected behaviours.
    #   # The problem is that rendering multiple courses could lead to a dispatch since
    #   # the second course creation recognizes that a knit-figures folder already
    #   # exists and takes the images from that one that are obviously not the
    #   # correct ones!
    #
    #   message("\nCleaning up \"", path.to.course, "\":")
    #
    #   cache.dir  = paste0(path.to.course, "/", file.name, "_cache")
    #   files.dir  = paste0(path.to.course, "/", file.name, "_files")
    #   figure.dir = paste0(path.to.course, "/knit-figure")
    #   tex.file   = paste0(path.to.course, "/", file.name, ".tex")
    #   html.file  = paste0(path.to.course, "/", file.name, ".html")
    #   libs.dir   = paste0(path.to.course, "/", "libs")
    #
    #   if ((! dir.exists(cache.dir)) &&
    #       (! dir.exists(files.dir)) &&
    #       (! dir.exists(figure.dir)) &&
    #       (! file.exists(tex.file)) &&
    #       (! file.exists(html.file))) {
    #     message("\t NOTHING TO CLEAN UP!")
    #   }
    #
    #   if (dir.exists(cache.dir)) {
    #     message("\t- Removing \"", file.name, "_cache\"")
    #     unlink(x = cache.dir, recursive = TRUE)
    #   }
    #   if (dir.exists(files.dir)) {
    #     message("\t- Removing \"", file.name, "_files\"")
    #     unlink(x = files.dir, recursive = TRUE)
    #   }
    #   if (dir.exists(figure.dir)) {
    #     message("\t- Removing \"knit-figure\"")
    #     unlink(x = figure.dir, recursive = TRUE)
    #   }
    #   if (dir.exists(libs.dir)) {
    #     message("\t- Removing \"libs\"")
    #     unlink(x = libs.dir, recursive = TRUE)
    #   }
    #   if (file.exists(tex.file)) {
    #     message("\t- Removing \"", file.name, ".tex\"")
    #     file.remove(tex.file)
    #   }
    #   if (file.exists(html.file)) {
    #     message("\t- Removing \"", file.name, ".html\"")
    #     file.remove(html.file)
    #   }
    #   cat("\n")
    # }

    # if (open.pdf) {
    #   PBSmodelling::openFile(paste0(path.to.course, "/", file.name, ".pdf"))
    # } else {
    #   # Return path to the created pdf:
    #   return(paste0(path.to.course, "/", file.name, ".pdf"))
    # }
  }

  attr(course.text, "file.path") = paste0(path.to.course, "/", file.name, ".pdf")

  # Return the course.text object:
  return(invisible(course.text))
}

#' @title Render just one slide set.
#'
#' @description
#'   This function creates and render just one slide set.
#'
#' @param slide.name [\code{character(1)}]\cr
#'   Name of the slide as given from the getStructure function.
#' @param open.pdf [\code{character(1)}]\cr
#'   A character vector telling whether the resulting PDF file should be opened. Default = FALSE.
#' @param ...
#'   Passed to `createCourse()`
#' @return [\code{character}]\cr
#'   A character vector containing the lines of the final Rmd file for the
#'   specific file named in 'slide.name'.
renderFile = function(slide.name, open.pdf = FALSE,
  xaringan = FALSE, template = "preamble_munichre.sty", ...) {
  return(createCourse(
    title = slide.name
    , course = "single_files"
    , file.name = paste0("single_rmd_", slide.name)
    , yaml.subtitle = ""
    , subtitles = NA
    , author = ""
    , course.date = ""
    , keep.source = FALSE
    , year = "slide"
    , month = slide.name
    , render = TRUE
    , xaringan = xaringan
    , course.list = list(list(slide.name))
    , open.pdf = open.pdf
    , template = template
    , ...
  ))
}


readSlide = function (slide_name, path = getwd()) {

  # Read the structure of the chapters or exercises:
  # struct = getStructure(type = "chapters")
  # struct[, slide := paste0("slides_", files)]
  # struct$files = NULL

  # Path to the rcoruses rep:
  path = here::here(glue::glue("chapters/slides_{slide_name}.Rmd"))

  # # Pathh to the slide:
  # path_to_slide = paste0(path,
  #   paste(struct[slide == paste0("slides_", slide_name)], collapse = "/"),
  #   ".Rmd")

  return(path)
}
