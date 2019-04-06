function(input,output){
  
  
  #   output$gif1 <- renderImage({
  #     
  #     outfile <- "~/diff-eq-simulations/blank.png"
  #     
  #     list(src = outfile,
  #          contentType = 'image/png',
  #          width = 2,
  #          height = 2)
  #   },deleteFile = F)
  #     
  #     
  # output$gif2 <- renderImage({
  #   outfile <- "~/diff-eq-simulations/blank.png"
  #   list(src = outfile,
  #        contentType = 'image/png',
  #        width = 2,
  #        height = 2)
  #   
  # },deleteFile = F)
  
  
  make_v_grid <- function(df,A){
    df$xend <- with(data = df,df$x +eval(parse(text = A[1])))
    df$yend <- with(data = df,df$y + eval(parse(text = A[2])))
    df$l = sqrt((df$xend - df$x)^2 + (df$yend - df$y)^2)
    
    df$x_short_end <- df$x + (df$xend - df$x) / df$l / 2
    df$y_short_end <- df$y + (df$yend - df$y) / df$l / 2
    
    
    
    df
    
    
  }
  
  
  find_closest_val <- function(df,pt){
    df$dist <- sqrt((df$x - pt[1])^2 + (df$y - pt[2])^2)
    df <- df %>% arrange(dist)
    df[1,]
    
  }
  
  make_trajectory <- function(A,pt,time,nframes,time_steps){
    
    
    n = time_steps
    dt = time/n
    df_pt <- data.frame(x = rep(pt[1],each = n), y = rep(pt[2],each = n))
    
    
    
    for(i in 1:(n - 1)){
      df_pt$x[i+1] <- with(data = df_pt,x[i] +eval(parse(text = A[1])) * dt)[i]
      df_pt$y[i+1] <- with(data = df_pt,y[i] +eval(parse(text = A[2])) * dt)[i]
    }
    
    indexes <- round(seq(from = 1, to = n, length.out = nframes))
    df_pt <- df_pt[indexes,]
    
    dt2 = time/nframes
    
    df_pt$n <- round(1:nrow(df_pt) * dt2,digits = 2)
    
    df_pt
  }
  
  
  fast_two_dof <- function(A,pt,time,nframes,time_steps){
    n = time_steps
    dt = time/n
    
    A1 <- parse(text = A[1])
    A2 <- parse(text = A[2])
    
    x <- pt[1]
    y <- pt[2]
    m <- mapply(function(a,b){
      xnew <- x + eval(A1)*dt
      ynew <- y + eval(A2)*dt
      x <<- xnew
      y <<- ynew
      c(xnew,ynew)
    } , 1:n, 1:n,SIMPLIFY = F)
    
    df <- data.frame(matrix(unlist(m),nrow = n,byrow = T))
    df <- rbind(data.frame(X1 = pt[1], X2 = pt[2]),df)
    names(df) <- c('x','y')
    df <- df[-nrow(df),]
    
    indexes <- round(seq(from = 1, to = n, length.out = nframes))
    df <- df[indexes,]
    
    dt2 <- time/nframes
    
    df$n <- round(1:nrow(df) * dt2,digits = 2)
    df
    
  }
  
  
  fast_four_dof <- function(A,pt,time,nframes,time_steps){
    n = time_steps
    dt = time/n
    
    A1 <- parse(text = A[1])
    A2 <- parse(text = A[2])
    A3 <- parse(text = A[3])
    A4 <- parse(text = A[4])
    
    x <- pt[1]
    y <- pt[2]
    xdot <- pt[3]
    ydot <- pt[4]
    
    m <- mapply(function(a,b){
      xnew <- x + eval(A1)*dt
      ynew <- y + eval(A2)*dt
      xdotnew <- xdot + eval(A3)*dt
      ydotnew <- ydot + eval(A4)*dt
      
      x <<- xnew
      y <<- ynew
      xdot <<- xdotnew
      ydot <<- ydotnew
      c(xnew,ynew,xdotnew,ydotnew)
    } , 1:n, 1:n,SIMPLIFY = F)
    
    df <- data.frame(matrix(unlist(m),nrow = n,byrow = T))
    df <- rbind(data.frame(X1 = pt[1], X2 = pt[2],X3 = pt[3],X4 = pt[4]),df)
    names(df) <- c('x','y','xdot','ydot')
    rownames(df) <- 1:nrow(df)
    df <- df[-nrow(df),]
    
    indexes <- round(seq(from = 1, to = n, length.out = nframes))
    df <- df[indexes,]
    
    dt2 <- time/nframes
    
    df$n <- round(1:nrow(df) * dt2,digits = 2)
    
    df
  }
  
  fast_six_dof <- function(A,pt,time,nframes,time_steps){
    n = time_steps
    dt = time/n
    
    A1 <- parse(text = A[1])
    A2 <- parse(text = A[2])
    A3 <- parse(text = A[3])
    A4 <- parse(text = A[4])
    A5 <- parse(text = A[5])
    A6 <- parse(text = A[6])
    
    x <- pt[1]
    y <- pt[2]
    z <- pt[3]
    xdot <- pt[4]
    ydot <- pt[5]
    zdot <- pt[6]
    
    m <- mapply(function(a,b){
      xnew <- x + eval(A1)*dt
      ynew <- y + eval(A2)*dt
      znew <- z + eval(A3)*dt
      xdotnew <- xdot + eval(A4)*dt
      ydotnew <- ydot + eval(A5)*dt
      zdotnew <- zdot + eval(A6)*dt
      
      x <<- xnew
      y <<- ynew
      z <<- znew
      xdot <<- xdotnew
      ydot <<- ydotnew
      zdot <<- zdotnew
      c(xnew,ynew,znew,xdotnew,ydotnew,zdotnew)
    } , 1:n, 1:n,SIMPLIFY = F)
    
    df <- data.frame(matrix(unlist(m),nrow = n,byrow = T))
    df <- rbind(data.frame(X1 = pt[1], X2 = pt[2],X3 = pt[3],X4 = pt[4],X5 = pt[5], X6 = pt[6]),df)
    names(df) <- c('x','y','z','xdot','ydot','zdot')
    rownames(df) <- 1:nrow(df)
    df[-nrow(df),]
  }
  
  
  general_traj <- function(A,pt,time,nframes,time_steps){
    n = time_steps
    nvar <- length(pt)
    dt = time/n
    
    var_names <- names(pt)
    
    
    for (i in 1:nvar){
      assign(var_names[i], pt[i],envir = globalenv())
    }
    
    govern_list <- lapply(A,function(x) parse(text = x))
    names(govern_list) <- var_names
    
    m <- mapply(function(aa,bb){
      new_vals <- sapply(govern_list, function(a) eval(a)*dt)
      old_vals <- sapply(var_names,function(a) get(a))
      new_vals <- old_vals + new_vals
      
      mapply(function(var_names,new_vals){
        assign(var_names,new_vals, envir = globalenv())
      }, var_names, new_vals)
      
      new_vals
      
    },1:(n-1),1:(n-1),SIMPLIFY = F)
    
    df <- data.frame(matrix(unlist(m),nrow = n-1,byrow = T))
    first_row <- t(as.data.frame(pt))
    row.names(first_row) <- NULL
    names(first_row) <- var_names
    names(df) <- var_names
    rbind(first_row,df) 
    
  }
  
  
  
  make_xy_trajectory <- function(df,L,time,nframes){
    
    dt <- time/nframes
    theta <- df$x + df$y * dt
    df <- data.frame(x = L*sin(theta) , y = -L*cos(theta) )
    df$n <- round(1:nrow(df) * dt,digits = 2)
    
    df
  }
  
  make_double_pendulum_trajectory <- function(df,l1,l2,time,nframes){
    # double pendulum
    dt <- time/nframes
    theta1 <- df$x + df$xdot * dt
    theta2 <- df$y +df$ydot*dt
    df <- data.frame(x1 = l1*sin(theta1) , y1 = -l1*cos(theta1), x2 = l1*sin(theta1) + l2*sin(theta2), y2 = -l1*cos(theta1) - l2*cos(theta2))
    df$n <- round(1:nrow(df) * dt,digits = 2)
    
    df
  }
  
  
  
  trim_df <- function(df,xx,yy) {
    
    df <- df %>% filter(x > xx[1], x <= xx[2],y > yy[1],y < yy[2])
    small_range <- round(seq(from = 1, to = nrow(df),length.out = 360))
    df <- df[sample(nrow(df)),]
    df[small_range,]
    
  }
  
  
  observeEvent(input$simulate_single_pendulum,{
    output$ui_single_pendulum_output <- renderUI({
      box(
        title = "", solidHeader = T, status = "primary",width = NULL,height = "900px",
        fluidRow(
          column(width = 12,
                 imageOutput(outputId = "single_pendulum_output",height = "100%")
                 )
        ),
        br(),
        br(),
        fluidRow(
          column(width = 12,
                 imageOutput(outputId = "single_pendulum_phase_trajectory")
                 )
        )
       
       

      )

      
    })
    
    withProgress(message = 'Simulating', value = 0, {
      

      nprog <- 3
      incProgress(1/nprog, detail = "Solving Diff Eq")
      l <- input$l
      mu <- input$mu
      theta <- input$theta * pi / 180
      theta_dot <- input$theta_dot * pi / 180
      
      
      time <- input$time
      fps <- switch (as.numeric(input$fps),5,10,20,30)
      nframes <- time * fps
      
      time_steps <- 80000
      
      a <- seq(from = -5*pi, to = 5*pi, length.out = 900)
      
      
      df <- expand.grid(x = a, y = a)
      
      
      
      
      
      A <- c("y",paste0(-mu,"*y - 9.81/",l,"*sin(x)"))
      
      df <- make_v_grid(df,A)
      
      
      
      # df_pt <- make_trajectory(A,c(theta,theta_dot),time,nframes,time_steps)
      # browser()
      df_pt <- fast_two_dof(A,c(theta,theta_dot),time,nframes,time_steps)
      minx <- ceiling(min(df_pt$x) /2 /pi) - 1
      maxx <- ceiling(max(df_pt$x) /2/ pi) + 1
      
      
      asmall <- seq(from = minx*2*pi, to = maxx*2*pi, length.out = 36)
      
      miny <- -10
      maxy <- 10
      if (min(df_pt$y) <= -10) miny <- min(df_pt$y) * 1.1
      if (max(df_pt$y) >= 10) miny <- max(df_pt$y) * 1.1
      
      
      
      asmally <- seq(from = miny, to = maxy, length.out = 36)
      df_small <- expand.grid(x = asmall, y  = asmally)
      df_small2 <- make_v_grid(df_small,A)
      
      incProgress(1/nprog, detail = "Creating Animation 1/2")
      
      g_traj <- ggplot() + geom_segment(data = df_small2,aes(x = x,y = y,xend = x_short_end, yend = y_short_end,color = l),arrow = arrow(length = unit(.1,"cm"))) + geom_hline(yintercept = 0) + 
        geom_vline(xintercept = 0) + scale_color_gradient(low = "blue",high = "red") + geom_line(data = df_pt, aes(x = x, y = y),size = 1) + ggtitle("Phase Space Trajectory")+
        xlim(minx*2*pi,maxx*2*pi) + ylim(-10,10) +theme_minimal() +theme(plot.title = element_text(size = 20),axis.title = element_text(size = 15))+ xlab("Angle")+ylab("Angular Velocity") +transition_reveal(along = n) + ease_aes('linear')
      
      
      
      a <- animate(g_traj,nframes = nframes,fps = fps)
      anim_save(filename = "vec_field.gif",animation = a,path = path_to_folder)

      #g <- ggplot(data = df_pt,aes(x = x, y = y)) + geom_point(size = 3)+ transition_time(n) + ease_aes('linear') + labs(title = 'Time Step: {frame_time}')
      #animate(g)
      
      
      
      df_pend <- make_xy_trajectory(df_pt,l,time,nframes)
      
      incProgress(1/nprog, detail = "Creating Animation 2/2")
      g <- ggplot(data = df_pend,aes(x = 0, y = 0,xend = x, yend = y)) + geom_segment(size = 3)+coord_fixed()+theme_minimal() + theme(plot.title = element_text(size = 20),axis.title = element_text(size = 15))+
        xlab("Position [m]")+ylab("Position [m]") +
        transition_time(n) + ease_aes('linear') + 
        labs(title = 'Time: {frame_time} s')
      
      a <- animate(g,nframes = nframes,fps = fps,height = 250)
      anim_save(filename = "traj.gif",animation = a,path = path_to_folder)
      
      output$single_pendulum_phase_trajectory<- renderImage(list(src =paste0(path_to_folder,"/vec_field.gif"),contentType = 'image/gif' ),deleteFile = T)
      output$single_pendulum_output <- renderImage(list(src =paste0(path_to_folder,"/traj.gif"),contentType = 'image/gif' ),deleteFile = T)
      

      
    })
  })
  
  observeEvent(input$simulate_double_pendulum,{
    
    output$ui_double_pendulum_output <- renderUI({
      box(
        title = "", solidHeader = T, status = "primary",width = NULL,height = "600px",
        fluidRow(
          column(width = 12,
                 imageOutput(outputId = "double_pendulum_output",height = "100%")
          )
        )
        
        
        
      )
      
      
    })
    
    
    
    withProgress(message = 'Simulating', value = 0, {
      
      
      nprog <- 2
      incProgress(1/nprog, detail = "Solving Diff Eq")
    
    l1 <- input$double_l1
    l2 <- input$double_l2
    m1 <- input$double_m1
    m2 <- input$double_m2
    g <- 9.81
    mu <- input$mu_double
    theta <- input$double_theta1 * pi / 180
    theta_dot <- input$double_theta_dot1 * pi / 180
    theta2 <- input$double_theta2* pi / 180
    theta_dot2 <- input$double_theta_dot2* pi / 180
    
    
    time <- input$double_time
    fps <- switch (as.numeric(input$double_fps),5,10,20,30)
    nframes <- time * fps
    
    time_steps <- 80000
    
    A1 <- paste0("1/(",(m1+m2)*l1,"-", m2*l1,"*cos(x - y)^2)*(-cos(x - y)*(",m2*l1,"*xdot^2*sin(x - y) - ",m2*g,"*sin(y))-",
                 m2*l2,"*ydot*sin(x - y) - ",g*(m1+m2),"*sin(x)) - ",mu,"*xdot")
    A2 <- paste0("1/(",m2*l2,"-",m2^2*l2/(m1+m2),"*cos(x - y)^2)*(",-m2/(m1+m2),"*cos(x-y)*(",-m2*l2,"*ydot^2*sin(x - y) -",g*(m1+m2),
                 "*sin(x))+",m2*l1,"*xdot^2*sin(x - y) -",m2*g,"*sin(y))- ",mu,"*ydot")
    
    A <- c("xdot","ydot",A1,A2)
    
    pt <- c(theta,theta2,theta_dot,theta_dot2)
    
    
    df_pt <- fast_four_dof(A,pt,time,nframes,time_steps)
    
    df_pend <- make_double_pendulum_trajectory(df_pt,l1,l2,time,nframes)
    incProgress(1/nprog, detail = "Creating Animation 1/1")
    g <- ggplot(data = df_pend,aes(x = 0, y = 0,xend = x1, yend = y1)) + geom_segment(size = 3,color = "blue")+ geom_segment(data = df_pend, aes(x = x1, y = y1, xend = x2, yend = y2),size = 3,color = "red")+
      coord_fixed()+theme_minimal() + theme(plot.title = element_text(size = 20),axis.title = element_text(size = 15)) + 
      xlab("Position [m]")+ylab("Position [m]") +
      transition_time(n) + ease_aes('linear') + 
      labs(title = 'Time: {frame_time} s')
    
    a <- animate(g,nframes = nframes,fps = fps)
    anim_save(filename = "traj.gif",animation = a,path = path_to_folder,height = 250)
    
    output$double_pendulum_output <- renderImage(list(src =paste0(path_to_folder,"/traj.gif"),contentType = 'image/gif' ),deleteFile = T)
    
    
    
    })
  })
  
}

