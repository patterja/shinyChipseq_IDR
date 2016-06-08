shinyServer(function(input, output) {

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Reproducible component


reprodInput <- reactive({
  switch(input$pkmethod,
         "MACS2"= ov_macs %>% 
           mutate(ksig=-10^(-karenina_macs$neglog10pval[ov_macs$Qindex]), msig=-10^(-monty_macs$neglog10pval[ov_macs$Sindex])),
         "BayesPeak" = ov_bayp %>% 
           mutate(ksig=karenina_bayp$PP[ov_bayp$Qindex], msig=monty_bayp$PP[ov_bayp$Sindex])
  )
})
#-------------------------------------------- Correlations Curves


output$corrCurve_macs <- renderPlot({
  ggplot(data=data.frame(x=uv_macs$psi$smoothed.line$x, y=uv_macs$psi$smoothed.line$y), aes(x, y)) +
    geom_line(size=0.8) +
    geom_point(data = data.frame(proportion=uv_macs$psi$t, psi=uv_macs$psi$value),
               aes(x=proportion, y=psi), size=4, shape=1) +
    geom_abline(intercept = 0, slope = 1, colour = "blue") +
    ggtitle("Correspondance Curve for CNV") +
    theme_bw() + 
    theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15), 
          axis.title.x = element_text(size=16), axis.title.y=element_text(size=16)) +
    xlab("proportion") + ylab("correlation (psi)")
})
output$corrCurve_bayp <- renderPlot({
  ggplot(data=data.frame(x=uv_bayp$psi$smoothed.line$x, y=uv_bayp$psi$smoothed.line$y), aes(x, y)) +
    geom_line(size=0.8) +
    geom_point(data = data.frame(proportion=uv_bayp$psi$t, psi=uv_bayp$psi$value),
               aes(x=proportion, y=psi), size=4, shape=1) +
    geom_abline(intercept = 0, slope = 1, colour = "blue") +
    ggtitle("Correspondance Curve for CNV") +
    theme_bw() + 
    theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15), 
          axis.title.x = element_text(size=16), axis.title.y=element_text(size=16)) +
    xlab("proportion") + ylab("correlation (psi)")
})

output$dcorrCurve_macs <- renderPlot({
  ggplot(data=data.frame(x=uv_macs$dpsi$smoothed.line$x, y=uv_macs$dpsi$smoothed.line$y), aes(x,y)) +
    geom_line(size=0.8, color="red") +
    geom_point(data = data.frame(proportion=uv_macs$dpsi$t, dpsi=uv_macs$dpsi$value),
               aes(x=proportion, y=dpsi), size=4, shape=1) +
    ggtitle("Derivative of the Correspondance Curve for CNV") +
    theme_bw() + 
    theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15), 
          axis.title.x = element_text(size=16), axis.title.y=element_text(size=16)) +
    xlab("proportion") + ylab("derivative correlation (dpsi)") + ylim(0,2)
  })

output$dcorrCurve_bayp <- renderPlot({
  ggplot(data=data.frame(x=uv_bayp$dpsi$smoothed.line$x, y=uv_bayp$dpsi$smoothed.line$y), aes(x,y)) +
    geom_line(size=0.8, color="red") +
    geom_point(data = data.frame(proportion=uv_bayp$dpsi$t, dpsi=uv_bayp$dpsi$value),
               aes(x=proportion, y=dpsi), size=4, shape=1) +
    ggtitle("Derivative of the Correspondance Curve for CNV") +
    theme_bw() + 
    theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15), 
          axis.title.x = element_text(size=16), axis.title.y=element_text(size=16)) +
    xlab("proportion") + ylab("derivative correlation (dpsi)") + ylim(0,2)
})
line<- reactive({
  
})
# df %>%
#   ggvis(~sigpeaks, ~IDR) %>%
#   layer_points(fill=~factor(name)) %>%
#   layer_lines(data=line, x= ~x, y= ~y)
# 
# bind_shiny("plot", "plot_ui")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SigPeaks plot
output$sigpeaks_plot<- renderPlot({
  
  
  ggplot(df, aes(x=sigpeaks, y=IDR, , colour=factor(name))) + 
    geom_point(size=2.0) +
    geom_hline(aes_string(yintercept=input$idrthresh), colour="#FF9999", size=1.5,alpha=0.6) +
    theme_bw() +
    theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),
          axis.title.x = element_text(size=16), axis.title.y=element_text(size=16), 
          legend.title = element_text(size=13),legend.text = element_text(size=13),
          plot.title = element_text(size = 16)) +
    scale_colour_discrete(name="Peak Caller") +
    xlab("Number of Significant Peaks") + ylab("IDR") + ggtitle("Number Peaks for IDR Thresholds")
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ data filtered and bound with idr
macs_idr<- reactive({
  data.frame(cbind(idr.out_macs$IDR, ov_macs, gibbon1=karenina_macs[ov_macs$Qindex,],gibbon2=monty_macs[ov_macs$Sindex,])) %>%
  filter(idr.out_macs.IDR <= input$idrthresh)
  
})
bayp_idr<- reactive({
  data.frame(cbind(idr.out_bayp$IDR, ov_bayp, gibbon1=karenina_bayp[ov_bayp$Qindex,],gibbon2=monty_bayp[ov_bayp$Sindex,])) %>%
  filter(idr.out_bayp.IDR <= input$idrthresh)
})

# ovfilt<- reactive({
#   macsidr<-reduce(makeGRangesFromDataFrame(macs_idr(), seqnames.field="space", start.field = "Qstart", end.field = "Qend"))
#   baypidr<-makeGRangesFromDataFrame(mutate(bayp_idr(), min_start=min(Qstart, Sstart), max_end=max(Qend, Send)), 
#                                     seqnames.field="space", start.field = "min_start", end.field = "max_end")
#   
#   olRanges(baypidr, macsidr)
# })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
output$reprodtable_macs<-renderDataTable({
  macs_idr() %>%
    select("IDR"=idr.out_macs.IDR, "chr"=space,
         gibbon1.start,gibbon1.end,gibbon1.neglog10qval, 
         gibbon2.start,gibbon2.end,gibbon2.neglog10qval,
         "overlap_type"=OLtype,
         "overlap%_gibb1"=OLpercQ,
         "overlap%_gibb2"=OLpercS)
          
})

output$reprodtable_bayp<-renderDataTable({
  bayp_idr() %>%
    select("IDR"=idr.out_bayp.IDR, "chr"=space,
           gibbon1.start,gibbon1.end,gibbon1.PP, 
           gibbon2.start,gibbon2.end,gibbon2.PP,
           "overlap_type"=OLtype,
           "overlap%_gibb1"=OLpercQ,
           "overlap%_gibb2"=OLpercS)

})
output$macs_stattable<- renderTable({
  macsidr<- macs_idr()
  
  data.frame(        
    Metric =c("Number of Gibbon Overlaps MACS",
              "Number of Peaks gibbon1 MACS",
              "Number of Peaks gibbon2 MACS",
              "Average Peak Size gibbon1",
              "Average Peak Size gibbon2"),
    
    "Filtered_Overlap" =c(nrow(macsidr),
                length(unique(macsidr$Qindex)),
                length(unique(macsidr$Sindex)),
                sprintf('%1.3f',as.numeric(mean(macsidr$Qend-macsidr$Qstart+1))),
                sprintf('%1.3f',mean(macsidr$Send-macsidr$Sstart+1))),
    All_Peaks =c(nrow(ov_macs),
                 nrow(karenina_macs),
                 nrow(monty_macs),
                 sprintf('%1.3f',mean(karenina_macs$end-karenina_macs$start+1)),
                 sprintf('%1.3f',mean(monty_macs$end-monty_macs$start+1)))
                  
    )})
output$bayp_stattable <- renderTable({
  baypidr<- bayp_idr()
  
  data.frame(        
    Metric =c("Number of Gibbon Overlaps BayesPeak",
              "Number of Peaks gibbon1 BayesPeak",
              "Number of Peaks gibbon2 BayesPeak",
              "Average Peak Size gibbon1",
              "Average Peak Size gibbon2"),
    
    "Filtered_Overlap" =c(nrow(baypidr),
                          length(unique(baypidr$Qindex)),
                          length(unique(baypidr$Sindex)),
                          sprintf('%1.3f',as.numeric(mean(baypidr$Qend-baypidr$Qstart+1))),
                          sprintf('%1.3f',mean(baypidr$Send-baypidr$Sstart+1))),
    All_Peaks =c(nrow(ov_bayp),
                 nrow(karenina_bayp),
                 nrow(monty_bayp),
                 sprintf('%1.3f',mean(karenina_bayp$end-karenina_bayp$start+1)),
                 sprintf('%1.3f',mean(monty_bayp$end-monty_bayp$start+1)))
    
  )})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})
