context("Preserve, Purge, and Restore Projects Functionality")

#####################################################################
# preserveProject                                                ####

test_that(
  "Preserve a project", 
  {
    ProjectList <- preserveProject(rcon)
    
    expect_list(ProjectList, 
                names = "named")
    
    expect_subset(names(ProjectList), 
                  choices = c("project_information", 
                              "arms", 
                              "events", 
                              "meta_data", 
                              "mappings", 
                              "repeating_instruments", 
                              "users", 
                              "user_roles", 
                              "user_role_assignments", 
                              "dags", 
                              "dag_assignments", 
                              "records"))
  }
)

test_that(
  "Preserve a project to an Rdata file", 
  {
    temp_dir <- tempdir()
    rdata_file <- file.path(temp_dir, 
                            sprintf("project-%s-RedcapList.Rdata", 
                                    rcon$projectInformation()$project_id))
    
    expect_true(preserveProject(rcon, 
                                save_as = "Rdata", 
                                dir = temp_dir, 
                                dir_create = TRUE))
    
    load(rdata_file)
    
    expect_no_error({
      suppressWarnings({
        off_con <- 
          offlineConnection(meta_data = RedcapList$meta_data, 
                            arms = RedcapList$arms, 
                            events = RedcapList$events,  
                            mapping = RedcapList$mappings, 
                            records = RedcapList$records, 
                            project_info = RedcapList$project_information)
      })
    })
    
    unlink(rdata_file)
    unlink(temp_dir)
  }
)

test_that(
  "Preserve a project to CSV files", 
  {
    temp_dir <- tempdir()
    
    expect_true(preserveProject(rcon, 
                                save_as = "csv", 
                                dir = temp_dir, 
                                dir_create = TRUE))
    
    csv_files <- file.path(temp_dir, 
                           sprintf("project-%s-%s.csv", 
                                   rcon$projectInformation()$project_id, 
                                   c("project_information", 
                                     "arms", 
                                     "events", 
                                     "meta_data", 
                                     "mappings", 
                                     "repeating_instruments", 
                                     "users", 
                                     "user_roles", 
                                     "user_role_assignments", 
                                     "dags", 
                                     "dag_assignments", 
                                     "records")))
    
    expect_no_error({
      suppressWarnings({
        off_con <- 
          offlineConnection(meta_data = csv_files[4], 
                            arms = csv_files[2], 
                            events = csv_files[3],  
                            mapping = csv_files[5], 
                            records = csv_files[12], 
                            project_info = csv_files[1])
      })
    })
    
    for (cf in csv_files){
      unlink(cf)
    }
    unlink(temp_dir)
  }
)
