quadrant <- function(row, column){
  centers = data.frame(center = 1:9, row = rep(c(2,5,8), each = 3), column = rep(c(2,5,8), 3))
  for(i in 1:nrow(centers)){
    centers$dist[i] = sqrt((row-centers[i,2])^2+(column-centers[i,3])^2)
  }
  return(centers$center[which.min(centers$dist)])
}

plot.sudoku <- function(df, savepng, picnum = NULL){
  if(savepng){
    png(paste("./sudoku/",gsub(":","_",Sys.time()),"_",picnum,".png",sep = ""),480,480)
  }
  correction = .128
  plot(NULL, ylim = c(1-correction,9+correction), xlim = c(1-correction,9+correction), yaxt = "n", xaxt = "n", xlab = "", ylab = "")
  abline(h = (1:8)+.5, v = (1:8)+.5, col = "grey")
  abline(h = (1:3)*3+.5, v = (1:3)*3+.5, col = "black")
  text(df$column, rev(df$row), df$value, col = c("red","black")[df$given+1])
  if(savepng){
    dev.off()
  }
}

n.solved.sudoku <- function(sudoku.df){
  return(sum(!is.na(sudoku.df$value)))
}

check.each.cell <- function(sudoku.df, candidates, savepng, picnum, sleep){
  
  n.solved = n.solved.sudoku(sudoku.df)
  
  #first step where we go through each cell and remove candidates 
  #which appear in their row/column/quadrant
  while(n.solved<81){
    for(i in 1:81){
      if(length(candidates[[i]])>1){
        friends = sort(unique(subset(sudoku.df, row == sudoku.df$row[i] | column == sudoku.df$column[i] | quadrant == sudoku.df$quadrant[i])$value))
        if(any(candidates[[i]] %in% friends)){
          candidates[[i]] = candidates[[i]][-which(candidates[[i]] %in% friends)]
        }
      }
      #if there's only 1 candidate left, add it to the data frame and remove it from their friends' candidates
      if(length(candidates[[i]])==1 & is.na(sudoku.df$value[i])){
        sudoku.df$value[i] = candidates[[i]]
        
        picnum = picnum + 1
        plot.sudoku(sudoku.df, savepng, picnum)
        if(!savepng){
          Sys.sleep(sleep)
        }
        
        friend.ids = subset(sudoku.df, row == sudoku.df$row[i] | column == sudoku.df$column[i] | quadrant == sudoku.df$quadrant[i])$id
        
        for(id in friend.ids){
          if(sudoku.df$value[i] %in% candidates[[id]]){
            candidates[[id]] = candidates[[id]][-which(candidates[[id]] == sudoku.df$value[i])]
          }
        }
      }
    }
    
    for(i in 1:81){

    }
    
    
    new.n.solved = n.solved.sudoku(sudoku.df)
    if(new.n.solved == n.solved){
      return(list(sudoku.df, candidates))
    }else{
      n.solved = new.n.solved
    }
    
  }
  return(list(sudoku.df, candidates))
}

check.each.system <- function(sudoku.df, candidates, savepng, picnum, sleep){
  
  n.solved = n.solved.sudoku(sudoku.df)
  
  while(n.solved<81){
    #second step where we find systems in which there is 
    #only one cell with a particular number as a candidate
    for(system in 4:2){
      #2 is rows
      #3 in columns
      #4 is quadrants
      for(i in 1:9){
        #these are the ids of the empty cells in the current system 
        target.ids = sudoku.df[sudoku.df[,system]==i & is.na(sudoku.df[,'value']) ,'id']
        #these are the possible values that can go in those cells
        candidate.table = sort(table(unlist(candidates[target.ids])))
        if(any(candidate.table==1)){
          #if there exists a number such that it is only a candidate for 
          #one cell in a given system, we can insert that number in that cell
          
          #this is the number to insert into a particular cell
          unique.number = as.numeric(names(candidate.table))[1]
          
          #this is the id of the cell to insert the number into
          target = target.ids[which(sapply(candidates[target.ids], function(x)unique.number %in% x))]
          
          #update the list of candidates
          candidates[[target]] = unique.number
          
          #update the data frame
          sudoku.df[target,'value'] = unique.number
          
          #plot progress
          picnum = picnum + 1
          plot.sudoku(sudoku.df, savepng, picnum)
          if(!savepng){
            Sys.sleep(sleep)
          }
          
          #update the friends' candidates
          friend.ids = subset(sudoku.df, row == sudoku.df$row[target] | column == sudoku.df$column[target] | quadrant == sudoku.df$quadrant[target])$id
          
          for(id in friend.ids){
            if(unique.number %in% candidates[[id]]){
              candidates[[id]] = candidates[[id]][-which(candidates[[id]] == unique.number)]
            }
          }
          
        }
      }
    }
    new.n.solved = n.solved.sudoku(sudoku.df)
    if(new.n.solved == n.solved){
      return(list(sudoku.df, candidates))
    }else{
      n.solved = new.n.solved
    }
    
  }
  return(list(sudoku.df, candidates))
}

solve.sudoku <- function(nums, rows, cols, savepng = T, sleep = .5){
  
  #check if the arguments are valid
  if((class(nums) != "numeric" &  class(nums) != "integer") || any(nums < 1) | any(nums > 9)){
    stop("Invalid input, 'nums' must be numeric vector with elements between 1 and 9")
  }
  if((class(rows) != "numeric" &  class(rows) != "integer") || any(rows < 1) | any(rows > 9)){
    stop("Invalid input, 'rows' must be numeric vector with elements between 1 and 9")
  }
  if((class(cols) != "numeric" &  class(cols) != "integer") || any(cols < 1) | any(cols > 9)){
    stop("Invalid input, 'nums' must be numeric vector with elements between 1 and 9")
  }
  
  #initialize data frame (1 row for each cell)
  sudoku.df = expand.grid(row = 1:9, column = 1:9, quadrant = NA, value = NA, given = FALSE)
  sudoku.df = cbind(data.frame(id = 1:81), sudoku.df)
  #assign  a quadrant to each cell
  for(i in 1:nrow(sudoku.df)){
    sudoku.df$quadrant[i] = quadrant(sudoku.df$row[i],sudoku.df$column[i])
  }
  
  #assign values to each cell which had initial values
  for(i in 1:length(nums)){
    sudoku.df$value[which(sudoku.df$row == rows[i] & sudoku.df$column == cols[i])] = nums[i]
    sudoku.df$given[which(sudoku.df$row == rows[i] & sudoku.df$column == cols[i])] = TRUE
  }
  
  #check if the input is a valid sudoku
  for(i in 1:9){
    if( any(duplicated(na.omit(subset(sudoku.df, quadrant==i)$value))) |
        any(duplicated(na.omit(subset(sudoku.df, row==i)$value))) |
        any(duplicated(na.omit(subset(sudoku.df, column==i)$value))) ){
      correction = .128
      plot(NULL, ylim = c(1-correction,9+correction), xlim = c(1-correction,9+correction), yaxt = "n", xaxt = "n", xlab = "", ylab = "")
      abline(h = (1:8)+.5, v = (1:8)+.5, col = "grey")
      abline(h = (1:3)*3+.5, v = (1:3)*3+.5, col = "black")
      text(sudoku.df$column, rev(sudoku.df$row), sudoku.df$value, col = c("red","black")[sudoku.df$given+1])
      stop("Invalid input. Each quadrant, row, and column must contain each integer exactly once.")
    }
  }
  
  #plot the initial grid
  picnum = 1
  plot.sudoku(sudoku.df, savepng, picnum)
  if(!savepng){
    Sys.sleep(sleep)
  }
  
  candidates = list()
  #populate the list of candidates for each cell
  for(i in 1:81){
    if(is.na(sudoku.df$value[i])){
      candidates[[i]] = 1:9
    }else{
      candidates[[i]] = sudoku.df$value[i]
    }
  }
  
  n.solved = n.solved.sudoku(sudoku.df)
  
  while(n.solved < 81){
    
    step1 = check.each.cell(sudoku.df, candidates, savepng, picnum, sleep)
    sudoku.df = step1[[1]]
    candidates = step1[[2]]
    
    n.solved = n.solved.sudoku(sudoku.df)
    if(n.solved == 81){
      break
    }
    
    step2 = check.each.system(sudoku.df, candidates, savepng, picnum, sleep)
    sudoku.df = step2[[1]]
    candidates = step2[[2]]
    
    n.solved = n.solved.sudoku(sudoku.df)
    if(n.solved == 81){
      break
    }
  }
  return(sudoku.df)
}

####examples

n = c(5,3,7,6,1,9,5,9,8,6,8,6,3,4,8,3,1,7,2,6,6,2,8,4,1,9,5,8,7,9)
r = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,7,7,7,8,8,8,8,9,9,9)
c = c(1,2,5,1,4,5,6,2,3,8,1,5,9,1,4,6,9,1,5,9,2,7,8,4,5,6,9,5,8,9)

solve = solve.sudoku(n,r,c,F)

n = c(9,6,4,5,3,8,7,2,1,5,3,6,9,7,2,8,4,1,3,1,8,2,5,5,4,8)
r = c(1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,6,6,6,7,7,8,8,8,9,9,9)
c = c(2,6,8,3,4,9,5,7,3,5,9,2,6,8,1,5,6,7,3,5,1,6,7,2,4,8)

solve = solve.sudoku(n,r,c,F)


#it can't solve this one
n = c(8,3,2,9,5,7,6,1,6,3,6,7,4,8,5,1,7,1,5,9,2,5,3,2,7,6)
r = c(1,1,1,1,2,2,2,3,3,4,5,5,5,5,5,5,6,7,7,8,8,8,9,9,9,9)
c = c(4,5,6,8,6,7,9,1,4,1,1,2,3,7,8,9,9,6,9,1,3,4,2,4,5,6)

solve = solve.sudoku(n,r,c,F,.1)


n = c(2,4,3,9,2,8,6,9,5,1,7,2,5,3,6,8,6,8,2,5,1,9,3,9,8,6)
r = c(1,1,1,2,2,2,3,3,3,4,5,5,5,5,5,5,6,7,7,7,8,8,8,9,9,9)
c = c(2,6,7,1,5,9,4,6,8,9,2,3,4,6,7,8,1,2,4,6,1,5,9,3,4,8)

solve.sudoku(n,r,c,F,.1)

#save the output to a gif
library(gifski)
save_gif(solve.sudoku(n,r,c,F,0), gif_file = "./sudoku.gif", width = 480, height = 480, delay = .01)
