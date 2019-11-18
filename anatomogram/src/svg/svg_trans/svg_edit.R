svg.edit <- function(path.in, path.out) {

  library(XML)
  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  style <- 'fill:#62fafa;fill-opacity:1;stroke:#000000;stroke-width:0.21602426;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
  out <- xmltop[[xmlSize(xmltop)-1]]; ply <- xmltop[[xmlSize(xmltop)]]
  # Remove base polygons to the top of the last group.
  if (xmlName(out)=='g') for (i in rev(seq_len(xmlSize(out)))) {

    if (xmlName(out[[i]])=='path'|xmlName(out[[i]])=='g') { addChildren(ply, kid=out[[i]], at=0) } 

  }; removeNodes(out) 

  # Change 'id' and 'style' of target polygons.
  for (i in seq_len(xmlSize(ply))) {
 
    if (is.null(ply[[i]][[1]])) addAttributes(ply[[i]], style=style)

    # Apply to original svg code downloaded from 'https://github.com/ebi-gene-expression-group/anatomogram/tree/master/src/svg'. Assign names to each path or group.
    if (any(names(ply[[i]])=='title')) { 
	  
      id <- xmlValue(ply[[i]][['title']]); addAttributes(ply[[i]], id=id) 
      removeNodes(ply[[i]][['title']]); addAttributes(ply[[i]], style=style)
  
    }

    if (xmlSize(ply[[i]])>=1) for (j in seq_len(xmlSize(ply[[i]]))) { addAttributes(ply[[i]][[j]], style=style) }

  }; for (i in seq_len(xmlSize(ply))) if (xmlName(ply[[i]])=='a') removeNodes(ply[[i]])

  # 'use' node outside 'g' group: Copy the referenced node and use it to replace the use node. Keep the 'id' of use node unchanged.
  w <- which(names(ply)=='use'); if (length(w)!=0) { 

    label <- NULL; for (i in seq_len(xmlSize(ply))) { label <- c(label, paste0('#', xmlGetAttr(ply[[i]], 'inkscape:label'))) }
    for (i in w) {
      
      # Keep attributes of 'use' node unchanged to avoid polygon shifting in position.
      nod <- ply[[i]]; att <- xmlAttrs(nod); na <- names(att); val <- as.character(att); names(val) <- na
      w1 <- which(label %in% xmlGetAttr(nod, 'xlink:href')); id <- xmlGetAttr(nod, 'id')
      ply[[i]] <- xmlClone(ply[[w1]], recursive=TRUE); addAttributes(ply[[i]], .attrs=val)

    }

  }

  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  saveXML(xmlfile, file=pa); cat(pa, '\n')
  
  # Check 'use' node inside 'g' group.
  w <- which(names(ply)=='g'); for (i in w) {

    nod <- ply[[i]]; w1 <- which(names(nod)=='use')
    if (length(w1)!=0) { cat(xmlGetAttr(nod, 'id'), ': \'use\' inside \'group\'', '\n') }

  }
  
  id <- NULL; for (i in seq_len(xmlSize(ply))) { id <- c(id, xmlGetAttr(ply[[i]], 'id')) }; return(id)

}

svg <- list.files(path='anatomogram/src/svg/', pattern='.svg$', full.names=T)
# for (i in svg[8]) svg.edit(i, '/home/jzhan067/spatialHeatmap/R/anatomogram/src/svg/svg_trans')



library(ExpressionAtlas); library(genefilter)
all.res <- searchAtlasExperiments(properties="brain", species="Mus musculus")
sl <- getAtlasData('E-GEOD-48962')[[1]][[1]]
df.exp <- assay(sl)
c.dat <- colData(sl)
c.dat.mus <- df.con <- as.data.frame(c.dat)
df.con$age <- sub(' week', 'w', df.con$age)
df.con$genotype <- sub(' transgenic', '.tran', df.con$genotype)
df.con$genotype <- sub('wild type', 'WT', df.con$genotype)
df.con$organism_part <- sub('brain ', '', df.con$organism_part)
tissue <- unique(df.con$organism_part) 
id <- svg.edit('brain.svg')
for (i in tissue) {

  w <- which(df.con$organism_part %in% i)
  df.con$organism_part[w] <- id[grep(i, id)]

}

con <- paste0(df.con$organism_part, '__', df.con$age, '.', df.con$genotype)
colnames(df.exp) <- con
avg <- t(aggregate(x=t(df.exp), by=list(colnames(df.exp)), FUN=mean)) 
colnames(avg) <- avg[1, ]; avg <- avg[-1, ]; rna <- rownames(avg)
avg <- apply(avg, 2, as.numeric); rownames(avg) <- rna
quantile(avg, probs=seq(0, 1, 0.25))
ffun <- filterfun(pOverA(1/ncol(avg), 80), cv(0.6, 1000))
filtered <- genefilter(avg, ffun); avg <- avg[filtered, ]; dim(avg)
write.table(avg, 'mouse_brain.txt', sep='\t', col.names=T, row.names=T)



library(ExpressionAtlas); library(genefilter)
all.res <- searchAtlasExperiments(properties="anther", species="brachypodium distachyon")
sl <- getAtlasData('E-MTAB-4401')[[1]][[1]]
df.exp <- assay(sl)
df.con.bra <- df.con <- as.data.frame(colData(sl))
df.con$developmental_stage <- gsub(' days after ', 'D.after.', df.con$developmental_stage)
df.con$developmental_stage <- gsub(' days before ', 'D.before.', df.con$developmental_stage)
df.con$developmental_stage <- gsub(', ', '.', df.con$developmental_stage)
df.con$developmental_stage <- gsub(' ', '.', df.con$developmental_stage)
df.con$organism_part <- make.names(df.con$organism_part)
tissue <- unique(df.con$organism_part) 
id <- svg.edit('flower.svg')
for (i in tissue) {

  w <- which(df.con$organism_part %in% i)
  id.sel <- id[grep(i, id)]
  if (length(id.sel)>=1) df.con$organism_part[w] <- id.sel

}

con <- paste0(df.con$organism_part, '__', df.con$developmental_stage)
colnames(df.exp) <- con
avg <- t(aggregate(x=t(df.exp), by=list(colnames(df.exp)), FUN=mean)) 
colnames(avg) <- avg[1, ]; avg <- avg[-1, ]; rna <- rownames(avg)
avg <- apply(avg, 2, as.numeric); rownames(avg) <- rna
quantile(avg, probs=seq(0, 1, 0.25))
ffun <- filterfun(pOverA(1/ncol(avg), 1000), cv(0.9, 10000))
filtered <- genefilter(avg, ffun); avg <- avg[filtered, ]; dim(avg)
write.table(avg, 'flower.txt', sep='\t', col.names=T, row.names=T)


library(ExpressionAtlas); library(genefilter)
all.res <- searchAtlasExperiments(properties="liver", species="gallus")
sl <- getAtlasData('E-MTAB-3724')[[1]][[1]]
df.exp <- assay(sl)
df.con.gal <- df.con <- as.data.frame(colData(sl))
df.con$organism_part <- gsub('skeletal muscle tissue', 'muscle', df.con$organism_part)
df.con$organism_part <- make.names(df.con$organism_part)
tissue <- unique(df.con$organism_part) 
id <- svg.edit('chicken.svg')
for (i in tissue) {

  w <- which(df.con$organism_part %in% i)
  id.sel <- id[grep(i, id)]
  if (length(id.sel)>=1) df.con$organism_part[w] <- id.sel

}

con <- paste0(df.con$organism_part, '__', df.con$AtlasAssayGroup)
colnames(df.exp) <- con
avg <- t(aggregate(x=t(df.exp), by=list(colnames(df.exp)), FUN=mean)) 
colnames(avg) <- avg[1, ]; avg <- avg[-1, ]; rna <- rownames(avg)
avg <- apply(avg, 2, as.numeric); rownames(avg) <- rna
quantile(avg, probs=seq(0, 1, 0.25))
ffun <- filterfun(pOverA(1/ncol(avg), 250), cv(1.0, 10000))
filtered <- genefilter(avg, ffun); avg <- avg[filtered, ]; dim(avg)
write.table(avg, 'chicken.txt', sep='\t', col.names=T, row.names=T)


library(ExpressionAtlas); library(genefilter)
all.res <- searchAtlasExperiments(properties="brain", species="macaca")
sl <- getAtlasData('E-MTAB-6813')[[1]][[1]]
df.exp <- assay(sl)
df.con.mac <- df.con <- as.data.frame(colData(sl))
df.con$organism_part <- gsub('skeletal muscle tissue', 'muscle', df.con$organism_part)
df.con$organism_part <- make.names(df.con$organism_part)
tissue <- unique(df.con$organism_part) 
id <- svg.edit('macaca.svg')
for (i in tissue) {

  w <- which(df.con$organism_part %in% i)
  id.sel <- id[grep(i, id)]
  if (length(id.sel)>=1) df.con$organism_part[w] <- id.sel

}

con <- paste0(df.con$organism_part, '__', df.con$AtlasAssayGroup)
colnames(df.exp) <- con
avg <- t(aggregate(x=t(df.exp), by=list(colnames(df.exp)), FUN=mean)) 
colnames(avg) <- avg[1, ]; avg <- avg[-1, ]; rna <- rownames(avg)
avg <- apply(avg, 2, as.numeric); rownames(avg) <- rna
quantile(avg, probs=seq(0, 1, 0.25))
ffun <- filterfun(pOverA(1/ncol(avg), 250), cv(1.0, 10000))
filtered <- genefilter(avg, ffun); avg <- avg[filtered, ]; dim(avg)
write.table(avg, 'chicken.txt', sep='\t', col.names=T, row.names=T)







