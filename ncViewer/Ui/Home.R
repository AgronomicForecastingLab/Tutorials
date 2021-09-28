tabPanel("Home", icon = icon("home"),
         useShinyjs(),
         fluidRow(
           column(2),
           column(8,  
                  HTML(paste0('
                                          <div class="jumbotron">
                                          <h1 class="display-3">Welcome to pSIMS Toolkit!</h1>
                                          <p class="lead">This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>
                                          <hr class="my-4">
                                          <p>It uses utility classes for typography and spacing to space content out within the larger container.</p>
                                          <p class="lead">
                                            
                                         ',
                              actionButton("new_run_btn","Start a new run !",class="btn btn-primary btn-lg"),
                              ' </p></div>')
                  )
           ),
           column(2)
         )
)