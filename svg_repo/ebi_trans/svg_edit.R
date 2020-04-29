svg.edit <- function(path.in, path.out) {

  library(xml2)
  doc <- read_xml(path.in); spa <- xml_attr(doc, 'space')
  if (!is.na(spa)) if (spa=='preserve') xml_set_attr(doc, 'xml:space', 'default')

  len <- xml_length(doc)
  out <- xml_children(doc)[[len-1]]; ply <- xml_children(doc)[[len]]; xml_set_attr(ply, 'id', 'container')
  # Move outline polygons to the top of the last group.
  if (xml_name(out)=='g') for (i in rev(seq_len(xml_length(out)))) {

    na <- xml_name(xml_children(out)[[i]])
    if (na=='path'|na=='g') { xml_add_child(.x=ply, .value=xml_children(out)[[i]], .where=0, .copy=FALSE) } 

  }; xml_remove(out, free=FALSE)

  # Change 'id' and 'style' of target polygons.
  style <- 'fill:#62fafa;fill-opacity:1;stroke:#000000;stroke-width:0.21602426;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1'
  chdn <- xml_children(ply); xml_set_attr(chdn, 'style', style)
  ids <- NULL; for (i in seq_len(xml_length(ply))) {

    # Apply to original svg code downloaded from 'https://github.com/ebi-gene-expression-group/anatomogram/tree/master/src/svg'. Assign names to each path or group.
    chdn1 <- xml_children(chdn[[i]]); na <- xml_name(chdn1)
    if (any(na=='title')) { 
	 
      tit <- chdn1[na=='title']
      
      id <- make.names(xml_attr(tit, 'id')) # Use title id as id.
      id2 <- make.names(xml_text(tit)) # Use title text as id.
      if (grepl('^title', id)) id <- id2
      xml_remove(tit, free=FALSE) # Only nodes with a title are assigned an "ontology". 
      ont <- xml_attr(chdn[[i]], 'id')
      xml_set_attr(chdn[[i]], 'id', id); xml_set_attr(chdn[[i]], 'ontology', ont)      
      names(id) <- ont; ids <- c(ids, id)
 
    }

  }; na <- xml_name(chdn); xml_remove(chdn[na=='a'], free=FALSE)  

  # 'use' node outside 'g' group: copy the referenced node and use it to replace the use node. Keep the 'id' of use node unchanged.
  w <- which(xml_name(chdn)=='use'); if (length(w)!=0) { 

    label <- NULL; for (i in seq_len(xml_length(ply))) { label <- c(label, paste0('#', xml_attr(chdn[[i]], 'ontology'))) }
    for (i in w) {
      
      # Keep attributes of 'use' node unchanged to avoid polygon shifting in position.
      nod <- chdn[[i]]; att <- xml_attrs(nod)
      w1 <- which(label %in% xml_attr(nod, 'href'))
      xml_replace(chdn[[i]], .value=chdn[[w1]], .copy=TRUE)
      d <- xml_attr(chdn[[w1]], 'd'); names(d) <- 'd'; att <- c(att, d)
      xml_set_attrs(xml_children(ply)[[i]], att) # This step uses complete attibutes from use node to overite copied node, so there is no "d". Thus "d" should be added.

    }

  }
 
  # Check 'use' node inside 'g' group.
  w <- which(xml_name(xml_children(ply))=='g'); for (i in w) {

    nod <- xml_children(ply)[[i]]
    w1 <- xml_name(xml_children(nod))=='use'
    if (any(w1)) { cat(xml_attr(nod, 'id'), ': \'use\' inside \'group\'', '\n') }

  }
  
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; pa <- paste0(path.out, '/', na)
  write_xml(doc, file=pa); cat(pa, '\n'); return(ids)

}

svg <- list.files(path='/home/jzhan067/SVG_tutorial_file/anatomogram/src/svg/', pattern='.svg$', full.names=T)
# In the svg tag, xml:space="preserve" introduces empty text nodes, e.g. mus_musculus.brain.svg. Thus this attribut should be removed.  
# Avoid running this function to all SVG images, since some images have dislocated polygons and need manual adjustment.
# for (i in svg[8]) svg.edit(path.in=i, path.out=path.out)

# In original EBI SVG, some tissues have label/ontology id while others donot, so should not rely on label to add the ontology attribute. 
svg1 <- list.files(path='/home/jzhan067/SVG_tutorial_file/svg_repo/ebi_trans', pattern='.svg$', full.names=T)
path.out <- '~/test/'

# Map ids/ontologys from SVGs resulting from svg.edit to these after maual adjustment. 
id.ont <- NULL; for (path.in in svg1) {

  doc <- read_xml(path.in); chdn <- xml_children(doc)
  ply <- chdn[[xml_length(doc)]]; chdn1 <- xml_children(ply)
  na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]
  path.in1 <- paste0(path.out, na) 
  doc1 <- read_xml(path.in1); chdn2 <- xml_children(doc1)
  ply1 <- chdn2[[xml_length(doc1)]]; chdn3 <- xml_children(ply1)  

  ids <- NULL; for (i in seq_along(chdn1)) {

    ont <- xml_attr(chdn1[[i]], 'ontology')
    id <- xml_attr(chdn1[[i]], 'id')
    for (j in seq_along(chdn3)) {

      ont1 <- xml_attr(chdn3[[j]], 'ontology')
      if (is.na(ont1)) next
      id1 <- xml_attr(chdn3[[j]], 'id') 
      if (is.na(ont) & !is.na(ont1)) {

        if (id==id1) {

          xml_set_attr(chdn1[[i]], 'ontology', ont1)
          names(id1) <- ont1; ids <- c(ids, id1); break        

        }

      }
      if (!is.na(ont)) if (ont==ont1) { 

        xml_attr(chdn1[[i]], 'id') <- id1
        names(id1) <- ont; ids <- c(ids, id1); break
      
      }

    }

  }; lis <- list(ids); names(lis) <- na; id.ont <- c(id.ont, lis)  
  write_xml(doc, file=path.in); cat(path.in, '\n')

}

# Add ontology id to example SVG images in spatialHeatmap.
library(xml2)
path1 <- '~/spatialHeatmap/inst/extdata/shinyApp/example/'
path2 <- '~/SVG_tutorial_file/svg_repo/ebi_trans/'
svg.pa1 <- list.files(path=path1, pattern='.svg$', full.names=TRUE)
svg.na1 <- list.files(path=path1, pattern='.svg$', full.names=FALSE)

for (i in seq_along(svg.pa1)) {

  svg.pa2 <- paste0(path2, svg.na1[i]); if (!file.exists(svg.pa2)) next
  doc1 <- read_xml(svg.pa1[i]); chdn1 <- xml_children(doc1)
  ply1 <- chdn1[[xml_length(doc1)]]; chdn2 <- xml_children(ply1)
  len1 <- xml_length(ply1)
  
  doc2 <- read_xml(svg.pa2); chdn3 <- xml_children(doc2)
  ply2 <- chdn3[[xml_length(doc2)]]; chdn4 <- xml_children(ply2)
  len2 <- xml_length(ply2)

  for (j in seq_len(len1)) {

    id1 <- make.names(xml_attr(chdn2[j], 'id'))
    xml_attr(chdn2[j], 'id') <- id1
    for (k in seq_len(len2)) {

      id2 <- xml_attr(chdn4[k], 'id')
      if (id1==id2) { xml_set_attr(chdn2[j], 'ontology', xml_attr(chdn4[k], 'ontology')); break }

    }

  }; write_xml(doc1, file=svg.pa1[i]); cat(svg.pa1[i], '\n')

}


library(xml2); library(rols)
return_feature <- function(feature, species, keywords.all=TRUE, remote=FALSE, dir=NULL, desc=FALSE, return.all=FALSE) {

  options(stringsAsFactors=FALSE)
  dir.check <- !is.null(dir) 
  if (dir.check) dir.check <- !(is.na(dir)) else stop("\'dir\' is not valid!") 
  if (dir.check) { dir.check <- dir.exists(dir); if (!dir.check) stop("\'dir\' is not valid!") } else stop("\'dir\'is not valid!")

  # Parse and return features.
  ftr.return <- function(svgs, desc=desc) { 

    cat('Accessing features... \n')
    id.ont <- NULL; for (path.in in svgs) {

      doc <- read_xml(path.in); chdn <- xml_children(doc)
      ply <- chdn[[xml_length(doc)]]; chdn1 <- xml_children(ply)
      na <- strsplit(path.in, '/')[[1]]; na <- na[grep('.svg$', na)]; len <- xml_length(ply)
      ids <- NULL; for (j in seq_len(len)) {

        ont <- xml_attr(chdn1[[j]], 'ontology')
        id <- xml_attr(chdn1[[j]], 'id'); names(id) <- ont
        ids <- c(ids, id)

       }; dup <- duplicated(ids)
       if (any(dup)) stop(paste0("Duplicated feature \'", paste0(ids[dup], collapse=', '), "\' detected in ", path.in, "!"))
       lis <- list(ids); names(lis) <- na; id.ont <- c(id.ont, lis) 

    }

    df <- NULL; for (i in seq_along(id.ont)) {

      feat <- id.ont[[i]]
      df0 <- data.frame(feature=feat, ontology=names(feat), row.names=NULL)
      df0$SVG <- names(id.ont[i]); df0$index <- as.numeric(rownames(df0))
      df <- rbind(df, df0)

    }; rownames(df) <- NULL

    if (desc==TRUE) {
  
      cat('Appending descriptions... \n')
      df$description <- NA; for (i in seq_len(nrow(df))) {

        ont <- df[i, 'ontology']; abbr <- tolower(sub('_.*', '', ont))
        trm <- tryCatch({ term(abbr, ont) }, error=function(e) { return(NA) })
        if (is(trm, 'Term')) { des <- termDesc(trm); if (!is.null(des)) df[i, 'description'] <- termDesc(trm) }
  
      }

    }; return(df)

  }

  if (remote==TRUE) {
  
    cat('Downloading SVG images... \n')
    tmp <- tempdir(check=TRUE); tmp1 <- paste0(tempdir(), '/git.zip')
    tmp2 <- paste0(tmp, '/git'); if (!dir.exists(tmp2)) dir.create(tmp2)
    download.file('https://github.com/jianhaizhang/SVG_tutorial_file/archive/master.zip', tmp1); unzip(tmp1, exdir=tmp2)
    tmp3 <- paste0(tmp2, '/SVG_tutorial_file-master/svg_repo')
    svgs <- list.files(path=tmp3, pattern='.svg$', full.names=TRUE, recursive=TRUE)
    df <- ftr.return(svgs=svgs, desc=desc)
    svgs.na <- sapply(svgs, function(i) { str <- strsplit(i, '/')[[1]]; str[length(str)] })
    svgs1 <- list.files(path=dir, pattern='.svg$', full.names=TRUE)
    svgs.na1 <- list.files(path=dir, pattern='.svg$', full.names=FALSE) 
    if (return.all==TRUE) { 


      svgs1.rm <- svgs1[svgs.na1 %in% svgs.na] 
      cat(paste0('Overwriting: ', svgs1.rm, '\n')); file.remove(svgs1.rm)   
      # "file.copy" does not overwrite.
      sapply(svgs, function (i) file.copy(i, dir)); return(df)

    }

  } else {

    svgs <- list.files(path=dir, pattern='.svg$', full.names=TRUE)
    df <- ftr.return(svgs=svgs, desc=desc)
    if (return.all==TRUE) return(df)

  }
  
  sp <- gsub(' |_|\\.|-|;|,', '|', species); ft <- gsub(' |_|\\.|-|;|,', '|', feature)
  
  if (keywords.all==TRUE) {

    sp <- strsplit(sp, '\\|')[[1]]; ft <- strsplit(ft, '\\|')[[1]]
    df.idx <- NULL; for (i in sp) {

     idx <- grepl(i, df$SVG, ignore.case=TRUE); df.idx <- cbind(df.idx, idx)

    } 
    for (i in ft) {

     idx <- grepl(i, df$feature, ignore.case=TRUE); df.idx <- cbind(df.idx, idx)

    } 
    w <- which(rowSums(df.idx)==ncol(df.idx))
    
  } else {

    w <- grepl(sp, df$SVG, ignore.case=TRUE) & grepl(ft, df$feature, ignore.case=TRUE)

  }; df1 <- df[w, ]; rownames(df1) <- NULL
  
  if (remote==TRUE) {

    svgs.cp <- svgs[svgs.na %in% df1$SVG]
    svgs.rm <- svgs1[svgs.na1 %in% df1$SVG]
    sapply(svgs.cp, function (i) file.copy(i, dir))
    if (dir.exists(tmp)) unlink(tmp, recursive=TRUE)
　　
  }; return(df1)

}


ft1 <- return_feature(feature='frontal cortex', species='homo sapiens', desc=FALSE, return.all=F, dir='~/test1', remote=F)

ft2 <- cbind(ft2=c('frontal.cortex', 'prefrontal.cortex', 'prefrontal.cortex', 'frontal.cortex', 'frontal.cortex', 'prefrontal.cortex'), ft1)
ft2 <- cbind(ft2=c('outline', 'outline', 'outline', 'outline2', 'outline3', 'outline4'), ft1)

feature=ft2; dir='~/test1'
library(xml2)
update_feature <- function(feature, dir) {

  dir.check <- !is.null(dir) 
  if (dir.check) dir.check <- !(is.na(dir)) else stop("\'dir\' is not valid!") 
  if (dir.check) { dir.check <- dir.exists(dir); if (!dir.check) stop("\'dir\' is not valid!") } else stop("\'dir\'is not valid!")
  feature[, 1] <- as.character(feature[, 1])
  feature$index <- as.numeric(feature$index)
  svgs.na <- unique(feature$SVG)
  for (i in svgs.na) {

    df0 <- subset(feature, SVG==i); dup <- duplicated(df0[, 1])
    if (any(dup)) stop(paste0("Duplicated feature \'", paste0(df0[, 1][dup], collapse=', '), "\' detected in ", i, "!"))
    path.in <- paste0(dir, '/', i)  
    doc <- read_xml(path.in); chdn <- xml_children(doc)
    ply <- chdn[[xml_length(doc)]]; chdn1 <- xml_children(ply)
    cat(paste0('Setting \'', paste0(df0[, 1], collapse=', '), '\' in ', path.in, '\n'))
    xml_set_attr(chdn1[df0$index], 'id', df0[, 1])
    write_xml(doc, file=path.in)
  
  }

}

update_feature(feature=ft2, dir='~/test1')





