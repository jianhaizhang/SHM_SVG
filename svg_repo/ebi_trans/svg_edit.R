svg.edit <- function(path.in, path.out) {

  library(XML)
  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  style <- 'fill:#62fafa;fill-opacity:1;stroke:#000000;stroke-width:0.21602426;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
  out <- xmltop[[xmlSize(xmltop)-1]]; ply <- xmltop[[xmlSize(xmltop)]]
  # Move base polygons to the top of the last group.
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
  # Name the container group.
  xmlAttrs(xmltop[[xmlSize(xmltop)]])[['id']] <- 'container'
  
  # Check 'use' node inside 'g' group.
  w <- which(names(ply)=='g'); for (i in w) {

    nod <- ply[[i]]; w1 <- which(names(nod)=='use')
    if (length(w1)!=0) { cat(xmlGetAttr(nod, 'id'), ': \'use\' inside \'group\'', '\n') }

  }
  
  id <- NULL; for (i in seq_len(xmlSize(ply))) { 
    
    id0 <- make.names(xmlGetAttr(ply[[i]], 'id'))
    xmlAttrs(ply[[i]])[['id']] <- id0
    id <- c(id, id0) 

  }; saveXML(xmlfile, file=pa); cat(pa, '\n'); return(id)

}

svg <- list.files(path='/home/jzhan067/SVG_tutorial_file/anatomogram/src/svg/', pattern='.svg$', full.names=T)
# Avoid running this function to all SVG images, since some images have dislocated polygons and need adjustment.
# for (i in svg[8]) svg.edit(path.in=i, path.out='/home/jzhan067/SVG_tutorial_file/anatomogram/src/svg/svg_trans')

# Name the container group.
library(XML)
svg1 <- list.files(path='/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans', pattern='.svg$', full.names=T)
path.out <- '/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans/'

for (i in svg1) {

  path.in <- i
  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  xmlAttrs(xmltop[[xmlSize(xmltop)]])[['id']] <- 'container'
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  saveXML(xmlfile, file=pa); cat(pa, '\n')

}

# Make names for ids.
for (path.in in svg1) {

  xmlfile <- xmlParse(path.in); xmltop <- xmlRoot(xmlfile)
  ply <- xmltop[[xmlSize(xmltop)]]
  for (j in seq_len(xmlSize(ply))) { 
    
    id0 <- make.names(xmlGetAttr(ply[[j]], 'id'))
    xmlAttrs(ply[[j]])[['id']] <- id0

  }
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  saveXML(xmlfile, file=pa); cat(pa, '\n')

}









