install.packages('tinytex')
tinytex::install_tinytex()


remotes::install_github('yihui/tinytex')
tinytex::install_tinytex(repository = "http://mirrors.tuna.tsinghua.edu.cn/CTAN/", version = "latest")
tinytex:::install_prebuilt()

tinytex::parse_install("/Users/pennekampster/Documents/Git projects/Swiss_SEM/5_Slides/Course_intro.log")
