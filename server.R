library(plotly)
library(shiny)
library(zoo)
library(pracma)
library(shinydashboard)
library(zeitgebr)
library(circular)
#########################################################################################
# Define server logic
shinyServer(function(input, output) {
  # output of wildtype data file
  output$contents1 <- renderTable({
    req(input$wildtype)
    df1<-read.delim(input$wildtype$datapath,
                   header=input$header,
                   sep=input$sep)
    # quote=input$quote)
    
    if(input$disp=="head"){
      return(head(df1))
    } else {
      return(df1)
    }
  })
  # output of mutant data file
  output$contents2 <- renderTable({
    req(input$mutant)
    df2<-read.delim(input$mutant$datapath,
                   header=input$header,
                   sep=input$sep)
    # quote=input$quote)
    
    if(input$disp=="head"){
      return(head(df2))
    } else {
      return(df2)
    }
  })
  
  # wildtype cycle wise profile
  output$wt_profiles <- renderPlotly({
    req(input$wildtype)
    df1<-read.delim(input$wildtype$datapath,
                    header=input$header,
                    sep=input$sep)
    profiles <- list()
    ind <- which(df1[,1] == df1[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df1[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    df1_forplot <- profiles[[input$cycle_no]]
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ax = list(
      title = "time index",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$modulotau*(60/input$bins)),
      tick0 = 0,
      dtick = (60/input$bins)*6
    )
    ay1 = list(
      title = "activity",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 4),
      tick0 = 0,
      dtick = 2
    )
    ay2 = list(
      title = "temperature",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(19, 29),
      tick0 = 20,
      dtick = 4,
      overlaying = "y",
      side = "right"
    )
    df1.plot <- plot_ly(
      type = 'scatter',
      mode = 'lines',
      height = 400,
      width = 520
    )
      for (i in 3:length(df1_forplot[1,])){
        df1.plot <- add_trace(
          df1.plot,
          x = seq(1, length(df1_forplot[,1])),
          y = df1_forplot[,i],
          line = list(
            width = 0.5,
            color = rgb(0, 0, 1, 0.5)
          ),
          showlegend = F
        )
      }
    df1.plot <- df1.plot%>%
      add_trace(
        x = seq(1, length(df1_forplot[,1])),
        y = df1_forplot[,2],
        line = list(
          color = 'red',
          width = 2
        ),
        yaxis = "y2",
        name = "Temperature"
      )%>%
      add_trace(
        x = seq(1, length(df1_forplot[,1])),
        y = rowMeans(df1_forplot[,-c(1:2)]),
        line = list(
          color = 'black',
          width = 3
        ),
        name = "Average profile"
      )%>%
      layout(
        showlegend = T,
        legend = list(
          x = 0.03,
          y = 1,
          bgcolor = rgb(0,0,0,0)
        ),
        xaxis = ax,
        yaxis = ay1,
        yaxis2 = ay2,
        autosize = F,
        margin = list(
          l = 50,
          r = 50,
          b = 100,
          t = 50,
          pad = 4
        )
      )
    
  })
  
  # mutant cycle wise profile
  output$mut_profiles <- renderPlotly({
    req(input$mutant)
    df2<-read.delim(input$mutant$datapath,
                    header=input$header,
                    sep=input$sep)
    profiles <- list()
    ind <- which(df2[,1] == df2[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df2[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    df2_forplot <- profiles[[input$cycle_no]]
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ax = list(
      title = "time index",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$modulotau*(60/input$bins)),
      tick0 = 0,
      dtick = (60/input$bins)*6
    )
    ay1 = list(
      title = "activity",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 4),
      tick0 = 0,
      dtick = 2
    )
    ay2 = list(
      title = "temperature",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(19, 29),
      tick0 = 20,
      dtick = 4,
      overlaying = "y",
      side = "right"
    )
    df2.plot <- plot_ly(
      type = 'scatter',
      mode = 'lines',
      height = 400,
      width = 520
    )
    for (i in 3:length(df2_forplot[1,])){
      df2.plot <- add_trace(
        df2.plot,
        x = seq(1, length(df2_forplot[,1])),
        y = df2_forplot[,i],
        line = list(
          width = 0.5,
          color = rgb(0, 0, 1, 0.5)
        ),
        showlegend = F
      )
    }
    df2.plot <- df2.plot%>%
      add_trace(
        x = seq(1, length(df2_forplot[,1])),
        y = df2_forplot[,2],
        line = list(
          color = 'red',
          width = 2
        ),
        yaxis = "y2",
        name = "Temperature"
      )%>%
      add_trace(
        x = seq(1, length(df2_forplot[,1])),
        y = rowMeans(df2_forplot[,-c(1:2)]),
        line = list(
          color = 'black',
          width = 3
        ),
        name = "Average profile"
      )%>%
      layout(
        showlegend = T,
        legend = list(
          x = 0.03,
          y = 1,
          bgcolor = rgb(0,0,0,0)
        ),
        xaxis = ax,
        yaxis = ay1,
        yaxis2 = ay2,
        autosize = F,
        margin = list(
          l = 50,
          r = 50,
          b = 100,
          t = 50,
          pad = 4
        )
      )
  })
  
  # wildtype cycle wise heatmaps
  output$wt_heatmaps <- renderPlotly({
    req(input$wildtype)
    df1_hm<-read.delim(input$wildtype$datapath,
                    header=input$header,
                    sep=input$sep)
    profiles <- list()
    ind <- which(df1_hm[,1] == df1_hm[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_hm[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    df1_forplot_hm <- profiles[[input$cycle_no_hm]]
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ax = list(
      title = "time",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$period),
      tick0 = 0,
      dtick = 4
    )
    ay = list(
      title = "individuals",
      autorange = 'reversed',
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 32),
      tick0 = 0,
      dtick = 4
    )
    
    index <- which(df1_hm[,1] == input$zt0)
    cyc <- list()
    
    for (i in 1:length(index)){
      cyc[[i]] <- as.matrix(df1_hm[index[i]:(index[i]+input$period*(60/input$bins)),])
    }
    
    slopes <- list()
    
    for (i in 1:length(cyc)){
      a = cyc[[i]]
      b = matrix(NA, nrow = length(a[,1]), ncol = length(a[1,]))
      for (j in 2:length(b[,1])){
        for (k in 1:length(b[1,])){
          b[j,k] = a[j,k]-a[j-1,k]
        }
      }
      slopes[[i]] <- b[,2:length(b[1,])]/b[,1]
    }
    
    df1_preforplot_hm <- slopes[[input$cycle_no_hm]]
    df1_forplot_hm <- df1_preforplot_hm[-c(1),-c(1)]

    df1.plot.hm <- plot_ly(
      type = 'heatmap',
      y = seq(1, length(df1_forplot_hm[1,]), by = 1),
      x = seq(1, input$period, by = input$bins/60),
      z = t(df1_forplot_hm),
      colors = colorRamp(c("white", "darkblue", "red")),
      zauto = F,
      zmin = 0,
      zmax = max(df1_preforplot_hm[,1], na.rm = T),
      height = 400,
      width = 520
    )%>%
      layout(
        xaxis = ax,
        yaxis = ay
      )
  })
  
  # mutant cycle wise heatmaps
  output$mut_heatmaps <- renderPlotly({
    req(input$mutant)
    df2_hm<-read.delim(input$mutant$datapath,
                       header=input$header,
                       sep=input$sep)
    profiles <- list()
    ind <- which(df2_hm[,1] == df2_hm[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_hm[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    df2_forplot_hm <- profiles[[input$cycle_no_hm]]
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ax = list(
      title = "time",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$period),
      tick0 = 0,
      dtick = 4
    )
    ay = list(
      title = "individuals",
      autorange = 'reversed',
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 32),
      tick0 = 0,
      dtick = 4
    )
    
    index <- which(df2_hm[,1] == input$zt0)
    cyc <- list()
    
    for (i in 1:length(index)){
      cyc[[i]] <- as.matrix(df2_hm[index[i]:(index[i]+input$period*(60/input$bins)),])
    }
    
    slopes <- list()
    
    for (i in 1:length(cyc)){
      a = cyc[[i]]
      b = matrix(NA, nrow = length(a[,1]), ncol = length(a[1,]))
      for (j in 2:length(b[,1])){
        for (k in 1:length(b[1,])){
          b[j,k] = a[j,k]-a[j-1,k]
        }
      }
      slopes[[i]] <- b[,2:length(b[1,])]/b[,1]
    }
    
    df2_preforplot_hm <- slopes[[input$cycle_no_hm]]
    df2_forplot_hm <- df2_preforplot_hm[-c(1),-c(1)]
    
    df2.plot.hm <- plot_ly(
      type = 'heatmap',
      y = seq(1, length(df2_forplot_hm[1,]), by = 1),
      x = seq(1, input$period, by = input$bins/60),
      z = t(df2_forplot_hm),
      colors = colorRamp(c("white", "darkblue", "red")),
      zauto = F,
      zmin = 0,
      zmax = max(df2_preforplot_hm[,1], na.rm = T),
      height = 400,
      width = 520
    )%>%
      layout(
        xaxis = ax,
        yaxis = ay
      )
  })
  
  # wildtype cycle wise slopes
  output$wt_slopes <- renderPlotly({
    req(input$wildtype)
    df1_sl<-read.delim(input$wildtype$datapath,
                       header=input$header,
                       sep=input$sep)
    profiles <- list()
    ind <- which(df1_sl[,1] == df1_sl[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_sl[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ax = list(
      title = "time",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$period),
      tick0 = 0,
      dtick = 4
    )

    index <- which(df1_sl[,1] == input$zt0)
    cyc <- list()
    
    for (i in 1:length(index)){
      cyc[[i]] <- as.matrix(df1_sl[index[i]:(index[i]+input$period*(60/input$bins)),])
    }
    
    slopes <- list()
    
    for (i in 1:length(cyc)){
      a = cyc[[i]]
      b = matrix(NA, nrow = length(a[,1]), ncol = length(a[1,]))
      for (j in 2:length(b[,1])){
        for (k in 1:length(b[1,])){
          b[j,k] = a[j,k]-a[j-1,k]
        }
      }
      slopes[[i]] <- b[,2:length(b[1,])]/b[,1]
    }
    
    # df1_preforplot_hm <- slopes[[input$cycle_no_hm]]
    # df1_forplot_hm <- df1_preforplot_hm[-c(1),-c(1)]
    
    plots <- list()
    
    for (i in 1:length(slopes)){
      pre_aa = slopes[[i]]
      aa = pre_aa
      
      plots[[i]] <- plot_ly(
        x = seq(0, input$period, by = input$bins/60),
        y = rowMeans(aa),
        type = "scatter",
        mode = "lines",
        line = list(
          width = 2,
          color = "black"
        ),
        name = "mean activity slope"
      )%>%
        add_trace(
          x = seq(0, input$period, by = input$bins/60),
          y = pre_aa[,1],
          type = "scatter",
          mode = "lines",
          line = list(
            width = 2,
            color = "red"
          ),
          name = "temperature slope"
        )%>%
        layout(
          showlegend = F,
          xaxis = ax,
          yaxis = list(
            title = paste("cycle", i, sep = ""),
            titlefont = f1,
            showticklabels = T,
            tickfont = f2,
            zeroline = T,
            showgrid = F,
            showline = T,
            linewidth = 2,
            linecolor = "black",
            ticklen = 7,
            tickwidth = 2,
            tickcolor = 'black',
            range = c(-0.4, 1.5),
            tick0 = 0,
            dtick = 0.5
          )
        )
    }
    
    df1.plot.sl <- subplot(plots, nrows = length(slopes),
                           shareX = T, titleY = T)
      
  })
  
  # mutant cycle wise slopes
  output$mut_slopes <- renderPlotly({
    req(input$mutant)
    df2_sl<-read.delim(input$mutant$datapath,
                       header=input$header,
                       sep=input$sep)
    profiles <- list()
    ind <- which(df2_sl[,1] == df2_sl[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_sl[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ax = list(
      title = "time",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$period),
      tick0 = 0,
      dtick = 4
    )
    
    index <- which(df2_sl[,1] == input$zt0)
    cyc <- list()
    
    for (i in 1:length(index)){
      cyc[[i]] <- as.matrix(df2_sl[index[i]:(index[i]+input$period*(60/input$bins)),])
    }
    
    slopes <- list()
    
    for (i in 1:length(cyc)){
      a = cyc[[i]]
      b = matrix(NA, nrow = length(a[,1]), ncol = length(a[1,]))
      for (j in 2:length(b[,1])){
        for (k in 1:length(b[1,])){
          b[j,k] = a[j,k]-a[j-1,k]
        }
      }
      slopes[[i]] <- b[,2:length(b[1,])]/b[,1]
    }
    
    # df1_preforplot_hm <- slopes[[input$cycle_no_hm]]
    # df1_forplot_hm <- df1_preforplot_hm[-c(1),-c(1)]
    
    plots <- list()
    
    for (i in 1:length(slopes)){
      pre_aa = slopes[[i]]
      aa = pre_aa
      
      plots[[i]] <- plot_ly(
        x = seq(0, input$period, by = input$bins/60),
        y = rowMeans(aa),
        type = "scatter",
        mode = "lines",
        line = list(
          width = 2,
          color = "black"
        ),
        name = "mean activity slope"
      )%>%
        add_trace(
          x = seq(0, input$period, by = input$bins/60),
          y = pre_aa[,1],
          type = "scatter",
          mode = "lines",
          line = list(
            width = 2,
            color = "red"
          ),
          name = "temperature slope"
        )%>%
        layout(
          showlegend = F,
          xaxis = ax,
          yaxis = list(
            title = paste("cycle", i, sep = ""),
            titlefont = f1,
            showticklabels = T,
            tickfont = f2,
            zeroline = T,
            showgrid = F,
            showline = T,
            linewidth = 2,
            linecolor = "black",
            ticklen = 7,
            tickwidth = 2,
            tickcolor = 'black',
            range = c(-0.4, 1.5),
            tick0 = 0,
            dtick = 0.5
          )
        )
    }
    
    df2.plot.sl <- subplot(plots, nrows = length(slopes),
                           shareX = T, titleY = T)
    
  })
  
  # wildtype average slopes
  output$wt_avg_slopes <- renderPlotly({
    req(input$wildtype)
    df1_avg<-read.delim(input$wildtype$datapath,
                       header=input$header,
                       sep=input$sep)
    profiles <- list()
    ind <- which(df1_avg[,1] == df1_avg[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_avg[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    
    index <- which(df1_avg[,1] == input$zt0)
    cyc <- list()
    
    for (i in 1:length(index)){
      cyc[[i]] <- as.matrix(df1_avg[index[i]:(index[i]+input$period*(60/input$bins)),])
    }
    
    slopes <- list()
    
    for (i in 1:length(cyc)){
      a = cyc[[i]]
      b = matrix(NA, nrow = length(a[,1]), ncol = length(a[1,]))
      for (j in 2:length(b[,1])){
        for (k in 1:length(b[1,])){
          b[j,k] = a[j,k]-a[j-1,k]
        }
      }
      slopes[[i]] <- b[,2:length(b[1,])]/b[,1]
    }
    
    for (i in 1:length(slopes)){
      pre_aa <- slopes[[i]]
      aa <- pre_aa[-c(1),-c(1)]
      slopes[[i]] <- aa
    }
    
    ax = list(
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black'
      # range = c(0,input$period),
      # tick0 = 0,
      # dtick = 4
    )
    ay = list(
      title = "slope",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(-0.4, 0.5),
      tick0 = 0,
      dtick = 0.5
    )
    
    df1.plots.avg <- plot_ly(
      type = "box"
    )
    for (i in 1:length(slopes)){
      df1.plots.avg <- df1.plots.avg%>%
        add_trace(
          y = colMeans(slopes[[i]], na.rm = T),
          fillcolor = rgb(0,1,0,0.6),
          line = list(
            color = "black",
            width = 1.5
          ),
          marker = list(
            color = rgb(0,1,0,1)
          ),
          name = paste("cycle", i, sep = "")
        )
    }
    df1.plots.avg <- df1.plots.avg%>%
      layout(
        showlegend = F,
        xaxis = ax,
        yaxis = ay
      )
  })
  
  # mutant average slopes
  output$mut_avg_slopes <- renderPlotly({
    req(input$mutant)
    df2_avg<-read.delim(input$mutant$datapath,
                        header=input$header,
                        sep=input$sep)
    profiles <- list()
    ind <- which(df2_avg[,1] == df2_avg[1,1])
    s_per_day = input$modulotau/(input$bins/60)
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_avg[ind[i]:(ind[i]+s_per_day)-1,]
    }
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    
    index <- which(df2_avg[,1] == input$zt0)
    cyc <- list()
    
    for (i in 1:length(index)){
      cyc[[i]] <- as.matrix(df2_avg[index[i]:(index[i]+input$period*(60/input$bins)),])
    }
    
    slopes <- list()
    
    for (i in 1:length(cyc)){
      a = cyc[[i]]
      b = matrix(NA, nrow = length(a[,1]), ncol = length(a[1,]))
      for (j in 2:length(b[,1])){
        for (k in 1:length(b[1,])){
          b[j,k] = a[j,k]-a[j-1,k]
        }
      }
      slopes[[i]] <- b[,2:length(b[1,])]/b[,1]
    }
    
    for (i in 1:length(slopes)){
      pre_aa <- slopes[[i]]
      aa <- pre_aa[-c(1),-c(1)]
      slopes[[i]] <- aa
    }
    
    ax = list(
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black'
      # range = c(0,input$period),
      # tick0 = 0,
      # dtick = 4
    )
    ay = list(
      title = "slope",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(-0.4, 0.5),
      tick0 = 0,
      dtick = 0.5
    )
    
    df2.plots.avg <- plot_ly(
      type = "box"
    )
    for (i in 1:length(slopes)){
      df2.plots.avg <- df2.plots.avg%>%
        add_trace(
          y = colMeans(slopes[[i]], na.rm = T),
          fillcolor = rgb(1,0,0,0.6),
          line = list(
            color = "black",
            width = 1.5
          ),
          marker = list(
            color = rgb(1,0,0,1)
          ),
          name = paste("cycle", i, sep = "")
        )
    }
    df2.plots.avg <- df2.plots.avg%>%
      layout(
        showlegend = F,
        xaxis = ax,
        yaxis = ay
      )
  })
  
  # wildtype piecewise linear regression slopes
  output$wt_line_sl <- renderPlotly({
    req(input$wildtype)
    df1_ln_sl<-read.delim(input$wildtype$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    raw <- df1_ln_sl[,-c(1,2)]
    
    pro <- matrix(0, nrow = s_per_day, ncol = length(raw[1,]))
    
    for (i in 1:length(raw[1,])){
      a=Reshape(raw[,i],s_per_day)
      mean_a<-as.matrix(rowMeans(a))
      pro[,i]<-mean_a
    }
    
    index <- which(df1_cyc[,1] == input$zt0)
    pro <- pro[index[1]:(index[1]+input$period*(60/input$bins)),]
    x <- seq(0, input$period, by = input$bins/60)
    y <- pro[,input$ind_no]
    
    breaks <- x[which(x >= 0 & x <= max(x))]
    
    mse <- numeric(length(breaks))
    for(i in 1:length(breaks)){
      piecewise1 <- lm(y ~ x*(x < breaks[i]) + x*(x>=breaks[i]))
      mse[i] <- summary(piecewise1)[6]
    }
    mse <- as.numeric(mse)
    
    b <- breaks[which(mse==min(mse))]
    
    piecewise2 <- lm(y ~ x*(x < b) + x*(x > b))
    z <- piecewise2$coefficients
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    
    ax = list(
      title = "time (ZT)",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$period+1),
      tick0 = 0,
      dtick = 4
    )
    ay = list(
      title = "activity",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 4),
      tick0 = 0,
      dtick = 2
    )
    
    p <- plot_ly(
      type = "scatter",
      mode = 'markers',
      x = x,
      y = y,
      marker = list(
        size = 6,
        symbol = 'circle',
        color = rgb(0,0,0,0.7),
        line = list(
          color = rgb(0,0,0,1),
          width = 1.5
        )
      ),
      name = "Raw"
    )%>%
      add_trace(
        x = seq(0,b, by = 30/60),
        y = as.numeric((z["(Intercept)"]+z["x < bTRUE"])) + as.numeric((z["x"] + z["x:x < bTRUE"]))*seq(0,b, by = 30/60),
        mode = "lines",
        line = list(
          width = 2,
          color = "red"
        ),
        marker = list(
          color = rgb(0,0,0,0),
          line = list(
            color = rgb(0,0,0,0)
          )
        ),
        text = paste("Slope = ",as.numeric((z["x"] + z["x:x < bTRUE"])), sep = ""),
        name = "Fit-1"
      )%>%
      add_trace(
        x = seq(b,max(x), by = 30/60),
        y = (as.numeric(z["(Intercept)"]+z["x > bTRUE"])) + as.numeric((z["x"]))*seq(b,max(x), by = 30/60),
        mode = "lines",
        marker = list(
          color = rgb(0,0,0,0),
          line = list(
            color = rgb(0,0,0,0)
          )
        ),
        line = list(
          width = 2,
          color = "green"
        ),
        text = paste("Slope = ",as.numeric((z["x"])), sep = ""),
        name = "Fit-2"
      )%>%
      add_segments(
        x = b, xend = b,
        y = 0, yend = 4,
        mode = 'lines',
        marker = list(
          color = rgb(0,0,0,0),
          line = list(
            color = rgb(0,0,0,0)
          )
        ),
        line = list(
          width = 2,
          color= 'gray',
          dash = "dot"
        ),
        text = b,
        name = paste("Break-point @ ", b, sep = "")
      )%>%
      layout(
        showlegend = T,
        legend = list(
          x = 0.03,
          y = 1,
          bgcolor = rgb(0,0,0,0)
        ),
        xaxis = ax,
        yaxis = ay
      )
    p
    
  })
  
  # mutant piecewise linear regression slopes
  output$mut_line_sl <- renderPlotly({
    req(input$mutant)
    df2_ln_sl<-read.delim(input$mutant$datapath,
                          header=input$header,
                          sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    raw <- df2_ln_sl[,-c(1,2)]
    
    pro <- matrix(0, nrow = s_per_day, ncol = length(raw[1,]))
    
    for (i in 1:length(raw[1,])){
      a=Reshape(raw[,i],s_per_day)
      mean_a<-as.matrix(rowMeans(a))
      pro[,i]<-mean_a
    }
    
    index <- which(df1_cyc[,1] == input$zt0)
    pro <- pro[index[1]:(index[1]+input$period*(60/input$bins)),]
    x <- seq(0, input$period, by = input$bins/60)
    y <- pro[,input$ind_no]
    
    breaks <- x[which(x >= 0 & x <= max(x))]
    
    mse <- numeric(length(breaks))
    for(i in 1:length(breaks)){
      piecewise1 <- lm(y ~ x*(x < breaks[i]) + x*(x>=breaks[i]))
      mse[i] <- summary(piecewise1)[6]
    }
    mse <- as.numeric(mse)
    
    b <- breaks[which(mse==min(mse))]
    
    piecewise2 <- lm(y ~ x*(x < b) + x*(x > b))
    z <- piecewise2$coefficients
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    
    ax = list(
      title = "time (ZT)",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0,input$period+1),
      tick0 = 0,
      dtick = 4
    )
    ay = list(
      title = "activity",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 4),
      tick0 = 0,
      dtick = 2
    )
    
    p <- plot_ly(
      type = "scatter",
      mode = 'markers',
      x = x,
      y = y,
      marker = list(
        size = 6,
        symbol = 'circle',
        color = rgb(0,0,0,0.7),
        line = list(
          color = rgb(0,0,0,1),
          width = 1.5
        )
      ),
      name = "Raw"
    )%>%
      add_trace(
        x = seq(0,b, by = 30/60),
        y = as.numeric((z["(Intercept)"]+z["x < bTRUE"])) + as.numeric((z["x"] + z["x:x < bTRUE"]))*seq(0,b, by = 30/60),
        mode = "lines",
        line = list(
          width = 2,
          color = "red"
        ),
        marker = list(
          color = rgb(0,0,0,0),
          line = list(
            color = rgb(0,0,0,0)
          )
        ),
        text = paste("Slope = ",as.numeric((z["x"] + z["x:x < bTRUE"])), sep = ""),
        name = "Fit-1"
      )%>%
      add_trace(
        x = seq(b,max(x), by = 30/60),
        y = (as.numeric(z["(Intercept)"]+z["x > bTRUE"])) + as.numeric((z["x"]))*seq(b,max(x), by = 30/60),
        mode = "lines",
        marker = list(
          color = rgb(0,0,0,0),
          line = list(
            color = rgb(0,0,0,0)
          )
        ),
        line = list(
          width = 2,
          color = "green"
        ),
        text = paste("Slope = ",as.numeric((z["x"])), sep = ""),
        name = "Fit-2"
      )%>%
      add_segments(
        x = b, xend = b,
        y = 0, yend = 4,
        mode = 'lines',
        marker = list(
          color = rgb(0,0,0,0),
          line = list(
            color = rgb(0,0,0,0)
          )
        ),
        line = list(
          width = 2,
          color= 'gray',
          dash = "dot"
        ),
        text = b,
        name = paste("Break-point @ ", b, sep = "")
      )%>%
      layout(
        showlegend = T,
        legend = list(
          x = 0.03,
          y = 1,
          bgcolor = rgb(0,0,0,0)
        ),
        xaxis = ax,
        yaxis = ay
      )
    p
    
  })
  
  # wildtype cycle wise CoM
  output$wt_cyc_com <- renderPlotly({
    req(input$wildtype)
    df1_cyc<-read.delim(input$wildtype$datapath,
                          header=input$header,
                          sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    profiles <- list()
    ind <- which(df1_cyc[,1] == df1_cyc[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_cyc[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df1_cyc[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    p <- list()
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
      
      col <- c("red","blue","green","gray","black","darkred","darkgreen",
               "darkblue","aquamarine","brown","cyan","darkmagenta","maroon",
               "gold","deeppink")
      plots<-plot_ly(
        r = as.vector(mean_r),
        theta = as.vector(mean_theta),
        type='scatterpolar',
        mode="markers",
        marker=list(color=col[i]),
        name = paste("Cycle-",i, sep = ""),
        height = 400,
        width = 510
      )
      p[[i]] <- plots
    }
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ang.axis = list(
      direction = "clockwise",
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black'
    )
    rad.axis = list(
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 1),
      tick0 = 0,
      dtick = 0.3
    )
    subplot(p)%>%
      layout(showlegend=FALSE,
             polar=list(
               angularaxis = ang.axis,
               radialaxis = rad.axis
             ),
             autosize = F,
             margin = list(
               l = 10,
               r = 10,
               b = 30,
               t = 30,
               pad = 4
             )
      )
  })
  
  # mutant cycle wise CoM
  output$mut_cyc_com <- renderPlotly({
    req(input$mutant)
    df2_cyc<-read.delim(input$mutant$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    profiles <- list()
    ind <- which(df2_cyc[,1] == df2_cyc[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_cyc[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df2_cyc[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    p <- list()
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
      
      col <- c("red","blue","green","gray","black","darkred","darkgreen",
               "darkblue","aquamarine","brown","cyan","darkmagenta","maroon",
               "gold","deeppink")
      plots<-plot_ly(
        r = as.vector(mean_r),
        theta = as.vector(mean_theta),
        type='scatterpolar',
        mode="markers",
        marker=list(color=col[i]),
        name = paste("Cycle-",i, sep = ""),
        height = 400,
        width = 510
      )
      p[[i]] <- plots
    }
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ang.axis = list(
      direction = "clockwise",
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black'
    )
    rad.axis = list(
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 1),
      tick0 = 0,
      dtick = 0.3
    )
    subplot(p)%>%
      layout(showlegend=FALSE,
             polar=list(
               angularaxis = ang.axis,
               radialaxis = rad.axis
             ),
             autosize = F,
             margin = list(
               l = 10,
               r = 10,
               b = 30,
               t = 30,
               pad = 4
             )
      )
  })
  
  # Download WT cyclewise CoM
  output$datatable.wt.cyc.com <- renderTable({
    req(input$wildtype)
    df1_cyc_com<-read.delim(input$wildtype$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    profiles <- list()
    ind <- which(df1_cyc_com[,1] == df1_cyc_com[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_cyc_com[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df1_cyc_com[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
    }
    
    m <- matrix(0, nrow = length(com.val[[1]]), ncol = length(com.val))
    
    for (i in 1:length(com.val)){
      for (j in 1:length(com.val[[1]])){
        m[j,i] = com.val[[i]][j]
      }
    }
    
    name.m <- vector()
    for (i in 1:length(m[1,])){
      if (i < length(ind)+1){
        name.m[i] <- c(paste("Phi.Cyc",i,sep = "")) 
      } else {
        name.m[i] <- c(paste("r.Cyc",i-length(ind),sep = ""))
      }
    }
    
    colnames(m) <- name.m
    wt_cyc_wise_com <<- m
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function(){
      paste("WT_cyclewise_CoM",".csv",sep="")
    },
    content = function(file){
      write.csv(wt_cyc_wise_com,file)
    }
  )
  
  # Download mutant cyclewise CoM
  output$datatable.mut.cyc.com <- renderTable({
    req(input$mutant)
    df2_cyc_com<-read.delim(input$mutant$datapath,
                            header=input$header,
                            sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    profiles <- list()
    ind <- which(df2_cyc_com[,1] == df2_cyc_com[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_cyc_com[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df2_cyc_com[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
    }
    
    m <- matrix(0, nrow = length(com.val[[1]]), ncol = length(com.val))
    
    for (i in 1:length(com.val)){
      for (j in 1:length(com.val[[1]])){
        m[j,i] = com.val[[i]][j]
      }
    }
    
    name.m <- vector()
    for (i in 1:length(m[1,])){
      if (i < length(ind)+1){
        name.m[i] <- c(paste("Phi.Cyc",i,sep = "")) 
      } else {
        name.m[i] <- c(paste("r.Cyc",i-length(ind),sep = ""))
      }
    }
    
    colnames(m) <- name.m
    mut_cyc_wise_com <<- m
  })
  
  
  output$downloadData2 <- downloadHandler(
    filename = function(){
      paste("Mutant_cyclewise_CoM",".csv",sep="")
    },
    content = function(file){
      write.csv(mut_cyc_wise_com,file)
    }
  )
  
  # wildtype average CoM
  output$wt_avg_com <- renderPlotly({
    req(input$wildtype)
    df1_cyc<-read.delim(input$wildtype$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    profiles <- list()
    ind <- which(df1_cyc[,1] == df1_cyc[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_cyc[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df1_cyc[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    p <- list()
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
    }
    
    m <- matrix(0, nrow = length(com.val[[1]]), ncol = length(com.val))
    
    for (i in 1:length(com.val)){
      for (j in 1:length(com.val[[1]])){
        m[j,i] = com.val[[i]][j]
      }
    }
    
    name.m <- vector()
    for (i in 1:length(m[1,])){
      if (i < length(ind)+1){
        name.m[i] <- c(paste("Phi.Cyc",i,sep = "")) 
      } else {
        name.m[i] <- c(paste("r.Cyc",i-length(ind),sep = ""))
      }
    }
    
    colnames(m) <- name.m
    
    mean_com <- matrix(0, nrow = length(m[,1]), ncol = 3)
    
    
    for (i in 1:(length(m[,1]))){
      mean_com[i,1] <- (as.numeric(mean.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,2] <- (as.numeric(sd.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,3] <- (as.numeric(rho.circular(m[i,1:length(ind)]*pi/180)))
    }
    
    colnames(mean_com) <- c("Phase", "SD", "Rho")
    rownames(mean_com) <- seq(1:length(mean_com[,1]))
    
    grand_mean <- matrix(0, nrow = 1, ncol = 3)
    
    grand_mean[1,1] <- as.numeric(mean.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,2] <- as.numeric(sd.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,3] <- as.numeric(rho.circular(mean_com[,1]*pi/180))
    
    colnames(grand_mean) <- c("Phase", "SD", "Rho")
    rownames(grand_mean)[1] <- "Grand mean"
    
    final_output <- rbind(mean_com,grand_mean)
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ang.axis = list(
      direction = "clockwise",
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black'
    )
    rad.axis = list(
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 1.2),
      tick0 = 0,
      dtick = 0.3
    )
    
    p <- plot_ly(
      r = rep(1.1, (length(final_output[,1])-1)),
      theta = as.vector(final_output[1:(length(final_output[,1])-1),1]),
      type = "scatterpolar",
      mode = 'markers',
      marker = list(
        size = 9,
        color = rgb(0,1,0,0.5),
        line = list(
          width = 1.5,
          color = rgb(0,1,0,1),
          dash = "dot"
        )
      ),
      name = "Individual CoM",
      height = 400,
      width = 510
    )%>%
      add_trace(
        r = c(0, final_output[length(final_output[,1]),3]),
        theta = c(final_output[length(final_output[,1]),1], final_output[length(final_output[,1]),1]),
        type = "scatterpolar",
        mode = 'lines',
        marker = list(
          size = 9,
          color = rgb(0,0,0,0),
          line = list(
            width = 1.5,
            color = rgb(0,0,0,0),
            dash = "dot"
          )
        ),
        line = list(
          color = rgb(0,1,0,1),
          width = 2
        ),
        name = "Mean CoM"
      )%>%
      layout(
        showlegend=FALSE,
        polar=list(
          angularaxis = ang.axis,
          radialaxis = rad.axis
        ),
        autosize = F,
        margin = list(
          l = 10,
          r = 10,
          b = 30,
          t = 30,
          pad = 4
        )
      )
  })
  
  # Download WT average CoM
  output$datatable.wt.avg.com <- renderTable({
    req(input$wildtype)
    df1_cyc<-read.delim(input$wildtype$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    profiles <- list()
    ind <- which(df1_cyc[,1] == df1_cyc[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df1_cyc[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df1_cyc[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    p <- list()
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
    }
    
    m <- matrix(0, nrow = length(com.val[[1]]), ncol = length(com.val))
    
    for (i in 1:length(com.val)){
      for (j in 1:length(com.val[[1]])){
        m[j,i] = com.val[[i]][j]
      }
    }
    
    name.m <- vector()
    for (i in 1:length(m[1,])){
      if (i < length(ind)+1){
        name.m[i] <- c(paste("Phi.Cyc",i,sep = "")) 
      } else {
        name.m[i] <- c(paste("r.Cyc",i-length(ind),sep = ""))
      }
    }
    
    colnames(m) <- name.m
    
    mean_com <- matrix(0, nrow = length(m[,1]), ncol = 3)
    
    
    for (i in 1:(length(m[,1]))){
      mean_com[i,1] <- (as.numeric(mean.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,2] <- (as.numeric(sd.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,3] <- (as.numeric(rho.circular(m[i,1:length(ind)]*pi/180)))
    }
    
    colnames(mean_com) <- c("Phase", "SD", "Rho")
    rownames(mean_com) <- seq(1:length(mean_com[,1]))
    
    grand_mean <- matrix(0, nrow = 1, ncol = 3)
    
    grand_mean[1,1] <- as.numeric(mean.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,2] <- as.numeric(sd.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,3] <- as.numeric(rho.circular(mean_com[,1]*pi/180))
    
    colnames(grand_mean) <- c("Phase", "SD", "Rho")
    rownames(grand_mean)[1] <- "Grand mean"
    
    final_output <- rbind(mean_com,grand_mean)
    wt_avg_com <<- final_output
  })
  
  output$downloadData3 <- downloadHandler(
    filename = function(){
      paste("WT_average_CoM",".csv",sep="")
    },
    content = function(file){
      write.csv(wt_avg_com,file)
    }
  )
  
  # mutant average CoM
  output$mut_avg_com <- renderPlotly({
    req(input$mutant)
    df2_cyc<-read.delim(input$mutant$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    profiles <- list()
    ind <- which(df2_cyc[,1] == df2_cyc[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_cyc[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df2_cyc[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    p <- list()
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
    }
    
    m <- matrix(0, nrow = length(com.val[[1]]), ncol = length(com.val))
    
    for (i in 1:length(com.val)){
      for (j in 1:length(com.val[[1]])){
        m[j,i] = com.val[[i]][j]
      }
    }
    
    name.m <- vector()
    for (i in 1:length(m[1,])){
      if (i < length(ind)+1){
        name.m[i] <- c(paste("Phi.Cyc",i,sep = "")) 
      } else {
        name.m[i] <- c(paste("r.Cyc",i-length(ind),sep = ""))
      }
    }
    
    colnames(m) <- name.m
    
    mean_com <- matrix(0, nrow = length(m[,1]), ncol = 3)
    
    
    for (i in 1:(length(m[,1]))){
      mean_com[i,1] <- (as.numeric(mean.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,2] <- (as.numeric(sd.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,3] <- (as.numeric(rho.circular(m[i,1:length(ind)]*pi/180)))
    }
    
    colnames(mean_com) <- c("Phase", "SD", "Rho")
    rownames(mean_com) <- seq(1:length(mean_com[,1]))
    
    grand_mean <- matrix(0, nrow = 1, ncol = 3)
    
    grand_mean[1,1] <- as.numeric(mean.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,2] <- as.numeric(sd.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,3] <- as.numeric(rho.circular(mean_com[,1]*pi/180))
    
    colnames(grand_mean) <- c("Phase", "SD", "Rho")
    rownames(grand_mean)[1] <- "Grand mean"
    
    final_output <- rbind(mean_com,grand_mean)
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 14,
      color = "black"
    )
    ang.axis = list(
      direction = "clockwise",
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = F,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black'
    )
    rad.axis = list(
      title = "",
      titlefont = f1,
      showticklabels = T,
      tickfont = f2,
      zeroline = T,
      showgrid = F,
      showline = T,
      linewidth = 2,
      linecolor = "black",
      ticklen = 7,
      tickwidth = 2,
      tickcolor = 'black',
      range = c(0, 1.2),
      tick0 = 0,
      dtick = 0.3
    )
    
    p <- plot_ly(
      r = rep(1.1, (length(final_output[,1])-1)),
      theta = as.vector(final_output[1:(length(final_output[,1])-1),1]),
      type = "scatterpolar",
      mode = 'markers',
      marker = list(
        size = 9,
        color = rgb(1,0,0,0.5),
        line = list(
          width = 1.5,
          color = rgb(1,0,0,1),
          dash = "dot"
        )
      ),
      name = "Individual CoM",
      height = 400,
      width = 510
    )%>%
      add_trace(
        r = c(0, final_output[length(final_output[,1]),3]),
        theta = c(final_output[length(final_output[,1]),1], final_output[length(final_output[,1]),1]),
        type = "scatterpolar",
        mode = 'lines',
        marker = list(
          size = 9,
          color = rgb(0,0,0,0),
          line = list(
            width = 1.5,
            color = rgb(0,0,0,0),
            dash = "dot"
          )
        ),
        line = list(
          color = rgb(1,0,0,1),
          width = 2
        ),
        name = "Mean CoM"
      )%>%
      layout(
        showlegend=FALSE,
        polar=list(
          angularaxis = ang.axis,
          radialaxis = rad.axis
        ),
        autosize = F,
        margin = list(
          l = 10,
          r = 10,
          b = 30,
          t = 30,
          pad = 4
        )
      )
  })
  
  # Download WT average CoM
  output$datatable.mut.avg.com <- renderTable({
    req(input$mutant)
    df2_cyc<-read.delim(input$mutant$datapath,
                        header=input$header,
                        sep=input$sep)
    s_per_day <- (60/input$bins)*input$modulotau
    
    profiles <- list()
    ind <- which(df2_cyc[,1] == df2_cyc[1,1])
    
    for (i in 1:length(ind)){
      profiles[[i]] <- df2_cyc[ind[i]:(ind[i]+s_per_day)-1,-c(2)]
    }
    theta<-as.matrix(df2_cyc[ind[i]:(ind[i]+s_per_day)-1,1]*360/input$modulotau)
    
    p <- list()
    com.val <- list()
    
    for (i in 1:length(profiles)){
      x = profiles[[i]]
      x = x[,-c(1,2)]
      
      sin_theta <- as.matrix(sin(theta*(pi/180)))
      cos_theta <- as.matrix(cos(theta*(pi/180)))
      fsintheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      fcostheta <- matrix(0,nrow=length(x[,1]),ncol=length(x[1,]))
      
      for (j in 1:length(x[,1])){
        for (k in 1:length(x[1,])){
          fsintheta[j,k] <- sin_theta[j,1]*x[j,k]
          fcostheta[j,k] <- cos_theta[j,1]*x[j,k]
        }
      }
      
      sum_fsintheta <- matrix(colSums(fsintheta),nrow=1)
      sum_fcostheta <- matrix(colSums(fcostheta),nrow=1)
      
      n_samples<-matrix(colSums(x),nrow=1)
      X <- matrix(0,nrow=1,ncol=length(x[1,]))
      Y <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_theta <- matrix(0,nrow=1,ncol=length(x[1,]))
      mean_r <- matrix(0,nrow=1,ncol=length(x[1,]))
      
      for (j in 1:length(n_samples[1,])){
        X[1,j] <- sum_fcostheta[1,j]/n_samples[1,j]
        Y[1,j] <- sum_fsintheta[1,j]/n_samples[1,j]
        if (X[1,j]<0){
          mean_theta[1,j] <- (pi + atan(Y[1,j]/X[1,j])) * 180/pi
        } else {
          mean_theta[1,j] <- (atan(Y[1,j]/X[1,j])) * 180/pi
        }
        mean_r[1,j] <- sqrt((X[1,j]^2) + (Y[1,j]^2))
      }
      
      com.val[[i]] <- as.vector(mean_theta)
      com.val[[i+length(ind)]] <- as.vector(mean_r)
    }
    
    m <- matrix(0, nrow = length(com.val[[1]]), ncol = length(com.val))
    
    for (i in 1:length(com.val)){
      for (j in 1:length(com.val[[1]])){
        m[j,i] = com.val[[i]][j]
      }
    }
    
    name.m <- vector()
    for (i in 1:length(m[1,])){
      if (i < length(ind)+1){
        name.m[i] <- c(paste("Phi.Cyc",i,sep = "")) 
      } else {
        name.m[i] <- c(paste("r.Cyc",i-length(ind),sep = ""))
      }
    }
    
    colnames(m) <- name.m
    
    mean_com <- matrix(0, nrow = length(m[,1]), ncol = 3)
    
    
    for (i in 1:(length(m[,1]))){
      mean_com[i,1] <- (as.numeric(mean.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,2] <- (as.numeric(sd.circular(m[i,1:length(ind)]*pi/180)))*180/pi
      mean_com[i,3] <- (as.numeric(rho.circular(m[i,1:length(ind)]*pi/180)))
    }
    
    colnames(mean_com) <- c("Phase", "SD", "Rho")
    rownames(mean_com) <- seq(1:length(mean_com[,1]))
    
    grand_mean <- matrix(0, nrow = 1, ncol = 3)
    
    grand_mean[1,1] <- as.numeric(mean.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,2] <- as.numeric(sd.circular(mean_com[,1]*pi/180))*180/pi
    grand_mean[1,3] <- as.numeric(rho.circular(mean_com[,1]*pi/180))
    
    colnames(grand_mean) <- c("Phase", "SD", "Rho")
    rownames(grand_mean)[1] <- "Grand mean"
    
    final_output <- rbind(mean_com,grand_mean)
    mut_avg_com <<- final_output
  })
  
  output$downloadData4 <- downloadHandler(
    filename = function(){
      paste("Mutant_average_CoM",".csv",sep="")
    },
    content = function(file){
      write.csv(mut_avg_com,file)
    }
  )
})