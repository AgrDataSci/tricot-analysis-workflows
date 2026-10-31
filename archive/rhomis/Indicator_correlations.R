####HH control score

Indicators <- NULL
Indicators <- cbind(rel_Female_control_energy,rel_Male_control_energy, score_HDDS_bad,score_HDDS_good,score_HFIAS,
                    score_PPI, Emissions_total)

Indicators$score_PPI <- na.omit(Indicators$score_PPI)
Indicators$rel_Female_control_energy <- na.omit(Indicators$rel_Female_control_energy)

Indicators <- as.data.frame(Indicators)

ind_corr <- round(cor(na.omit(Indicators)),2)

lower.tri(ind_corr, diag = FALSE)
upper.tri(ind_corr, diag = FALSE)

#Hide upper triangle
upper<-ind_corr
upper[upper.tri(ind_corr)]<-""
upper<-as.data.frame(upper)
upper

library(xtable)
require(Hmisc)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
corr_sign <- corstars(Indicators)
library(ReporteRs)
corr_indicators = docx()
table = FlexTable(data=corr_sign, add.rownames = TRUE)
doc = addFlexTable(corr_indicators, table)
writeDoc(corr_indicators, file="correlations.docx")
