model_run <- function() {

  n = 100; a = 1; b = 1; tau = 100
  z = rnorm(n)
  eta = a + b*z

  scale = exp(rnorm(n))
  prec = scale*tau
  y = rnorm(n, mean = eta, sd = 1/sqrt(prec))


  data = list(y=y, z=z)
  formula = y ~ 1+z
  result = inla(formula, family = "gaussian", data = data, verbose = TRUE)

  output <- summary(result)


  return((flatten_list(output)))
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}


#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
