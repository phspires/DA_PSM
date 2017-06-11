##CREATE THE INNER MODEL
##CONECTION MATRIX BETWEEN LATENT VARIABLES


#the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
create.conection.matrix=function (innermodel,outermodel){
  
  if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
  order=c()
  attach(innermodel)
  
  conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
  
  for (i in 1:ncol(conection)){
    order[i]=get.number.mv(outermodel)[i,1]
  }
  
  colnames(conection)=order
  rownames(conection)=order
    
  for (i in 1:ncol(conection)){
    new_data=innermodel[which(FROM==order[i]),]
    FROM, TO

The following objects are masked from innermodel (pos = 10):

    FROM, TO

The following objects are masked from innermodel (pos = 11):

    FROM, TO

The following objects are masked from inner_m:

    FROM, TO

Called from: eval(expr, envir, enclos)
Browse[1]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#19: colnames(conection) = order
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#20: rownames(conection) = order
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#22: for (i in 1:ncol(conection)) {
    new_data = innermodel[which(FROM == order[i]), ]
    for (j in i:ncol(conection)) {
        d = colnames(conection)[j]
        if (colnames(conection)[j] %in% new_data[, 2]) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    if (colnames(conection)[j] %in% new_data[, 2]) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#26: if (colnames(conection)[j] %in% new_data[, 2]) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#26: if (colnames(conection)[j] %in% new_data[, 2]) {
    conection[i, j] = 1
}
Browse[2]> View(new_data)
Browse[2]> View(new_data)
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> Q
> if(colnames(conection)[j] %in% as.vector(new_data[,2])){
+       conection[i,j]=1
+       
+       }
Error in is.data.frame(x) : object 'conection' not found
> ##CREATE THE INNER MODEL
> ##CONECTION MATRIX BETWEEN LATENT VARIABLES
> 
> 
> #the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
> create.conection.matrix=function (innermodel,outermodel){
+   
+   if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
+   order=c()
+   attach(innermodel)
+   
+   conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
+   
+   for (i in 1:ncol(conection)){
+     order[i]=get.number.mv(outermodel)[i,1]
+   }
+ 
+   
+   colnames(conection)=order
+   rownames(conection)=order
+     
+   for (i in 1:ncol(conection)){
+     new_data=innermodel[which(FROM==order[i]),]
+     for (j in i:ncol(conection)){
+       d=colnames(conection)[j]
+       if(colnames(conection)[j] %in% as.vector(new_data[,2])){
+       conection[i,j]=1
+       
+       }
+     
+       
+     }
+     
+   }
+ }
> source('~/DA_PSM_NEW/conection_matrix.R', encoding = 'UTF-8')
> create.conection.matrix(inner_m,outer.m)
The following objects are masked from innermodel (pos = 3):

    FROM, TO

The following objects are masked from innermodel (pos = 4):

    FROM, TO

The following objects are masked from innermodel (pos = 5):

    FROM, TO

The following objects are masked from innermodel (pos = 6):

    FROM, TO

The following objects are masked from innermodel (pos = 7):

    FROM, TO

The following objects are masked from innermodel (pos = 8):

    FROM, TO

The following objects are masked from innermodel (pos = 9):

    FROM, TO

The following objects are masked from innermodel (pos = 10):

    FROM, TO

The following objects are masked from innermodel (pos = 11):

    FROM, TO

The following objects are masked from innermodel (pos = 12):

    FROM, TO

The following objects are masked from inner_m:

    FROM, TO

Called from: eval(expr, envir, enclos)
Browse[1]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#19: colnames(conection) = order
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#20: rownames(conection) = order
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#22: for (i in 1:ncol(conection)) {
    new_data = innermodel[which(FROM == order[i]), ]
    for (j in i:ncol(conection)) {
        d = colnames(conection)[j]
        if (colnames(conection)[j] %in% as.vector(new_data[, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    if (colnames(conection)[j] %in% as.vector(new_data[, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#26: if (colnames(conection)[j] %in% as.vector(new_data[, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#26: if (colnames(conection)[j] %in% as.vector(new_data[, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#26: if (colnames(conection)[j] %in% as.vector(new_data[, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> View(new_data)
Browse[2]> View(new_data)
Browse[2]> 'LOYALTY' %in% new_data[,2]
[1] FALSE
Browse[2]> 'LOYALTY' %in% new_data[3,2]
[1] TRUE
Browse[2]> is.vector(new_data)
[1] FALSE
Browse[2]> is.matrix(new_data)
[1] FALSE
Browse[2]> is.data.frame(new_data)
[1] TRUE
Browse[2]> new_data= as.vector(new_datas)
Error in as.vector(new_datas) : object 'new_datas' not found
Called from: as.vector(new_datas)
Browse[3]> new_data= as.vector(new_data)
Error during wrapup: object 'new_data' not found
Browse[3]> Q
> ##CREATE THE INNER MODEL
> ##CONECTION MATRIX BETWEEN LATENT VARIABLES
> 
> 
> #the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
> create.conection.matrix=function (innermodel,outermodel){
+   
+   if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
+   order=c()
+   attach(innermodel)
+   
+   conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
+   
+   for (i in 1:ncol(conection)){
+     order[i]=get.number.mv(outermodel)[i,1]
+   }
+ 
+   
+   colnames(conection)=order
+   rownames(conection)=order
+     
+   for (i in 1:ncol(conection)){
+     new_data=innermodel[which(FROM==order[i]),]
+     for (j in i:ncol(conection)){
+       d=colnames(conection)[j]
+       
+       for (k in (1:nrow(new_data))){
+         if(colnames(conection)[j] %in% as.vector(new_data[k,2])){
+           conection[i,j]=1
+         }
+       
+       }
+     
+       
+     }
+     
+   }
+ }
> create.conection.matrix(inner_m,outer.m)
The following objects are masked from innermodel (pos = 3):

    FROM, TO

The following objects are masked from innermodel (pos = 4):

    FROM, TO

The following objects are masked from innermodel (pos = 5):

    FROM, TO

The following objects are masked from innermodel (pos = 6):

    FROM, TO

The following objects are masked from innermodel (pos = 7):

    FROM, TO

The following objects are masked from innermodel (pos = 8):

    FROM, TO

The following objects are masked from innermodel (pos = 9):

    FROM, TO

The following objects are masked from innermodel (pos = 10):

    FROM, TO

The following objects are masked from innermodel (pos = 11):

    FROM, TO

The following objects are masked from innermodel (pos = 12):

    FROM, TO

The following objects are masked from innermodel (pos = 13):

    FROM, TO

The following objects are masked from inner_m:

    FROM, TO

> ##CREATE THE INNER MODEL
> ##CONECTION MATRIX BETWEEN LATENT VARIABLES
> 
> 
> #the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
> create.conection.matrix=function (innermodel,outermodel){
+   
+   if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
+   order=c()
+   attach(innermodel)
+   
+   conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
+   
+   for (i in 1:ncol(conection)){
+     order[i]=get.number.mv(outermodel)[i,1]
+   }
+ 
+   
+   colnames(conection)=order
+   rownames(conection)=order
+     
+   for (i in 1:ncol(conection)){
+     new_data=innermodel[which(FROM==order[i]),]
+     for (j in i:ncol(conection)){
+       d=colnames(conection)[j]
+       
+       for (k in (1:nrow(new_data))){
+         if(colnames(conection)[j] %in% as.vector(new_data[k,2])){
+           conection[i,j]=1
+         }
+       
+       }
+     
+       
+     }
+     
+   }
+ }
> source('~/DA_PSM_NEW/conection_matrix.R', encoding = 'UTF-8')
> create.conection.matrix(inner_m,outer.m)
The following objects are masked from innermodel (pos = 3):

    FROM, TO

The following objects are masked from innermodel (pos = 4):

    FROM, TO

The following objects are masked from innermodel (pos = 5):

    FROM, TO

The following objects are masked from innermodel (pos = 6):

    FROM, TO

The following objects are masked from innermodel (pos = 7):

    FROM, TO

The following objects are masked from innermodel (pos = 8):

    FROM, TO

The following objects are masked from innermodel (pos = 9):

    FROM, TO

The following objects are masked from innermodel (pos = 10):

    FROM, TO

The following objects are masked from innermodel (pos = 11):

    FROM, TO

The following objects are masked from innermodel (pos = 12):

    FROM, TO

The following objects are masked from innermodel (pos = 13):

    FROM, TO

The following objects are masked from innermodel (pos = 14):

    FROM, TO

The following objects are masked from inner_m:

    FROM, TO

Called from: eval(expr, envir, enclos)
Browse[1]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#19: colnames(conection) = order
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#20: rownames(conection) = order
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#22: for (i in 1:ncol(conection)) {
    new_data = innermodel[which(FROM == order[i]), ]
    for (j in i:ncol(conection)) {
        d = colnames(conection)[j]
        for (k in (1:nrow(new_data))) {
            if (colnames(conection)[j] %in% as.vector(new_data[k, 
                2])) {
                conection[i, j] = 1
            }
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    for (k in (1:nrow(new_data))) {
        if (colnames(conection)[j] %in% as.vector(new_data[k, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> View(conection)
Browse[2]> View(conection)
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    for (k in (1:nrow(new_data))) {
        if (colnames(conection)[j] %in% as.vector(new_data[k, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    for (k in (1:nrow(new_data))) {
        if (colnames(conection)[j] %in% as.vector(new_data[k, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    for (k in (1:nrow(new_data))) {
        if (colnames(conection)[j] %in% as.vector(new_data[k, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> View(conection)
Browse[2]> View(conection)
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    for (k in (1:nrow(new_data))) {
        if (colnames(conection)[j] %in% as.vector(new_data[k, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#29: conection[i, j] = 1
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#23: new_data = innermodel[which(FROM == order[i]), ]
Browse[2]> View(conection)
Browse[2]> View(conection)
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#24: for (j in i:ncol(conection)) {
    d = colnames(conection)[j]
    for (k in (1:nrow(new_data))) {
        if (colnames(conection)[j] %in% as.vector(new_data[k, 
            2])) {
            conection[i, j] = 1
        }
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#25: d = colnames(conection)[j]
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#27: for (k in (1:nrow(new_data))) {
    if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
        conection[i, j] = 1
    }
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
debug at ~/DA_PSM_NEW/conection_matrix.R#28: if (colnames(conection)[j] %in% as.vector(new_data[k, 2])) {
    conection[i, j] = 1
}
Browse[2]> n
> ##CREATE THE INNER MODEL
> ##CONECTION MATRIX BETWEEN LATENT VARIABLES
> 
> 
> #the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
> create.conection.matrix=function (innermodel,outermodel){
+   
+   if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
+   order=c()
+   attach(innermodel)
+   
+   conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
+   
+   for (i in 1:ncol(conection)){
+     order[i]=get.number.mv(outermodel)[i,1]
+   }
+ 
+   
+   colnames(conection)=order
+   rownames(conection)=order
+     
+   for (i in 1:ncol(conection)){
+     new_data=innermodel[which(FROM==order[i]),]
+     for (j in i:ncol(conection)){
+       d=colnames(conection)[j]
+       
+       for (k in (1:nrow(new_data))){
+         if(colnames(conection)[j] %in% as.vector(new_data[k,2])){
+           conection[i,j]=1
+         }
+       
+       }
+     
+       
+     }
+     
+   }
+   
+   return(conection)
+ }
> ##CREATE THE INNER MODEL
> ##CONECTION MATRIX BETWEEN LATENT VARIABLES
> 
> 
> #the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
> create.conection.matrix=function (innermodel,outermodel){
+   
+   if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
+   order=c()
+   attach(innermodel)
+   
+   conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
+   
+   for (i in 1:ncol(conection)){
+     order[i]=get.number.mv(outermodel)[i,1]
+   }
+   
+   colnames(conection)=order
+   rownames(conection)=order
+     
+   for (i in 1:ncol(conection)){
+     new_data=innermodel[which(FROM==order[i]),]
+     for (j in i:ncol(conection)){
+       d=colnames(conection)[j]
+       
+       for (k in (1:nrow(new_data))){
+         if(colnames(conection)[j] %in% as.vector(new_data[k,2])){
+           conection[i,j]=1
+         }
+       
+       }
+     }
+   }
+   return(conection)
+ }
> create.conection.matrix(inner_m,outer.m)
The following objects are masked from innermodel (pos = 3):

    FROM, TO

The following objects are masked from innermodel (pos = 4):

    FROM, TO

The following objects are masked from innermodel (pos = 5):

    FROM, TO

The following objects are masked from innermodel (pos = 6):

    FROM, TO

The following objects are masked from innermodel (pos = 7):

    FROM, TO

The following objects are masked from innermodel (pos = 8):

    FROM, TO

The following objects are masked from innermodel (pos = 9):

    FROM, TO

The following objects are masked from innermodel (pos = 10):

    FROM, TO

The following objects are masked from innermodel (pos = 11):

    FROM, TO

The following objects are masked from innermodel (pos = 12):

    FROM, TO

The following objects are masked from innermodel (pos = 13):

    FROM, TO

The following objects are masked from innermodel (pos = 14):

    FROM, TO

The following objects are masked from innermodel (pos = 15):

    FROM, TO

The following objects are masked from inner_m:

    FROM, TO

             IMAGE EXPECTATION QUALITY VALUE SATISFACTION LOYALTY
IMAGE            0           1       0     0            1       1
EXPECTATION      0           0       1     1            1       0
QUALITY          0           0       0     1            1       0
VALUE            0           0       0     0            1       0
SATISFACTION     0           0       0     0            0       1
LOYALTY          0           0       0     0            0       0
> 
    for (j in i:ncol(conection)){
      d=colnames(conection)[j]
      
      for (k in (1:nrow(new_data))){
        if(colnames(conection)[j] %in% as.vector(new_data[k,2])){
          conection[i,j]=1
        }
      
      }
    }
  }
  return(conection)
}