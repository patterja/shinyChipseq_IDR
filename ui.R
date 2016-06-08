shinyUI(navbarPage("IDR Analysis",
 tabPanel("IDR Comparisons", 
          numericInput("idrthresh", 
                       label = h5(paste0("IDR Threshold\nvalue between 0 and 0.572")), 
                       value = 0.01), submitButton("Submit"),
          plotOutput("sigpeaks_plot", width =900),
          
          fluidRow(
            column(6, h4("MACS2"), tableOutput("macs_stattable")),
            column(6, h4("BayesPeak"), tableOutput("bayp_stattable"))
          ),
          #sliderInput("idrthresh", "IDR Threshold:",min = 0, max = 0.6, value = 0.01))),
          tabsetPanel(
            tabPanel("MACS2",dataTableOutput("reprodtable_macs")),
            tabPanel("BayesPeak",dataTableOutput("reprodtable_bayp"))
           
          )),
  tabPanel("Correspondence Curves",
    fluidRow(
      column(6, plotOutput("corrCurve_macs", height=200, width=400), 
             plotOutput("dcorrCurve_macs")),
      column(6, plotOutput("corrCurve_bayp", height=200, width=400), 
             plotOutput("dcorrCurve_bayp"))))  
 
  
  )
)
