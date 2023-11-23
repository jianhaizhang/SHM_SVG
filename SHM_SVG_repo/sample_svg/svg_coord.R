library(rsvg); library(grImport)

# Issue 1: In zea_mays.whole_plant1.svg, there are two paths. But in test1.ps.xml, only one path is extracted while the other is lost.
svg.p1 <- 'zea_mays.whole_plant1.svg'
rsvg_ps(svg.p1, file='test1.ps'); PostScriptTrace('test1.ps', 'test1.ps.xml')

# Issue 2: In sorghum_bicolor.flower_parts1.svg there are two paths. In test2.ps.xml one fill and one stroke are needed for each path in SVG. But actually in test2.ps.xml, one fill is lost.
svg.p2 <- 'sorghum_bicolor.flower_parts1.svg'
rsvg_ps(svg.p2, file='test2.ps'); PostScriptTrace('test2.ps', 'test2.ps.xml')
