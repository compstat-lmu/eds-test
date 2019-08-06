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
  add_step(step_setup_push_deploy(branch = "gh-pages", orphan = TRUE)) %>%

  # delete old contents if they exist
  add_step(step_run_code(file.remove("courses/basis/eds-test/*"))) %>%

  add_step(step_run_code(source("create-course.R"))) %>%
  add_step(step_run_code(source("basis-course.R"))) %>%
  add_step(step_run_code(source("courses/basis/2019_03/render-course.R"))) %>%
  add_step(step_run_code(rmarkdown::render_site("courses/basis/2019_03"))) %>%
  # encrypt course html file
  add_step(step_run_code(encryptedRmd::encrypt_html_file("courses/basis/eds-test/basis_descriptive.html",
                                                         output_path = "courses/basis/eds-test/basis_descriptive.html",
                                                         write_key_file = TRUE))) %>%

  # finally push everything
  add_step(step_run_code(file.copy(dir("courses/basis/eds-test/", full.names = TRUE), "docs/", recursive = TRUE))) %>%
  add_step(step_do_push_deploy(path = "docs"))
