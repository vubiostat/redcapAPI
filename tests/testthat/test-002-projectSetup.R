context("Initialize Test Project")

purgeProject(rcon, 
             purge_all = TRUE)

load(test_path("testdata", "test_redcapAPI_MetaData.Rdata"))

importMetaData(rcon, 
               test_redcapAPI_MetaData[1, ])
