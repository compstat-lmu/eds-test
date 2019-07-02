get_stage("install") %>%
  add_step(step_run_code(rownames(utils::installed.packages()))) %>%
  add_step(step_run_code(getwd())) %>%
  add_step(step_run_code(dir())) %>%
  add_step(step_run_code(remotes::install_deps())) %>%
  add_step(step_run_code(rownames(utils::installed.packages())))

get_stage("deploy") %>%
  add_step(step_add_to_known_hosts("github.com")) %>%
  add_step(step_install_ssh_keys()) %>%
  add_step(step_test_ssh()) %>%
  add_step(step_setup_push_deploy(path = "docs", branch = "gh-pages")) %>%
  add_step(step_run_code(source("create-course.R"))) %>%
  add_step(step_run_code(source("basis-course.R"))) %>%
  add_step(step_run_code(source("courses/basis/2019_03/render-course.R"))) %>%
  #add_step(step_run_code(withr::with_dir("website", rmarkdown::render_site()))) %>%
  #add_step(step_run_code(withr::with_dir("landing-page", rmarkdown::render_site()))) %>%
  add_step(step_run_code(file.copy("courses/basis/2019_03/basis_descriptive.html", "docs", overwrite = T))) %>%
  add_step(step_do_push_deploy(path = "docs"))
