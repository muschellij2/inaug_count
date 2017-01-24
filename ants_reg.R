# https://www.nytimes.com/interactive/2017/01/20/us/politics/trump-inauguration-crowd.html
# http://www.nydailynews.com/news/national/reuters-editor-defends-photo-president-trump-inauguration-article-1.2953125
# http://abcnews.go.com/Politics/2009-2017-comparing-trumps-obamas-inauguration-crowds/story?id=44927217
rm(list = ls())
library(RNiftyReg)
library(png)
library(EBImage)
library(jpeg)
library(animation)
library(abind)

# obama = readPNG("obama.png")
# trump = readPNG("trump.png")

obama_stub = "abc_obama"
trump_stub = "abc_trump_resized_black"
ext = ".jpg"
# obama_stub = "obama"
# trump_stub = "trump"
# ext = ".png"


func = switch(ext, 
    ".jpg" = readJPEG,
    ".png" = readPNG)
obama = func(paste0(obama_stub, ext))
obama = as(obama, "array")
trump = func(paste0(trump_stub, ext))
trump = as(trump, "array")


make_png_name = function(stub, ending, 
                         sep = "_",
                         ext = ".png") {
  paste0(paste(stub, ending, sep = sep), ext)
}

obama = obama[,, 1:3]
obama = as(obama, "array")
obama_orig = apply(obama, 1:2, mean)
writePNG(obama_orig, 
         target = make_png_name(obama_stub, "orig_bw"))

obama[obama == 0] = NA
obama_bw <- apply(obama, 1:2, mean)
writePNG(obama_bw, 
    target = make_png_name(obama_stub, "bw"))

trump = trump[,, 1:3]
trump_orig <- apply(trump, 1:2, mean)
writePNG(obama_orig, 
         target = make_png_name(obama_stub, "orig_bw"))
trump[trump == 0] = NA
trump_bw <- apply(trump, 1:2, mean)
writePNG(trump_bw, 
    target =  make_png_name(trump_stub, "bw"))

affine = niftyreg(
    source = trump_bw, 
    target = obama_bw)
 
out_aff = as(affine$image, "array")
writePNG(out_aff,
    target = make_png_name(trump_stub, "bw_affine"))

reg_aff = applyTransform(trump_orig, transform = forward(affine))

writePNG(reg_aff,
         target = make_png_name(trump_stub, "orig_bw_affine"))

color_aff = apply(trump, 3, function(x) {
  y = applyTransform(x, transform = forward(affine))
  y = as(y, "array")
  y[ y > 1 & !is.na(y)] = 1
  y[ y < 0 & !is.na(y)] = 0
  y = list(y)
  return(y)
})
color_aff = lapply(color_aff, function(x) x[[1]])
arr = array(dim = c(dim(reg_aff), 3))
for (i in 1:3) {
  arr[,,i] = color_aff[[i]]
}
color_aff = arr
writePNG(color_aff,
         target = make_png_name(trump_stub, "color_affine"))

# out = niftyreg(
#     source = trump_bw, 
#     target = obama_bw,
#     scope = "nonlinear",
#     init = forward(affine))
# out_nonlin_withna = as(out$image, "array")
# writePNG(out_nonlin_withna, 
#     target = make_png_name(trump_stub, "bw_nonlin_with_NA"))


keep_cols = colMeans(reg_aff < 0.01) < 0.95
keep_rows = rowMeans(reg_aff < 0.01) < 0.95

start = color_aff[keep_rows, keep_cols, ]
end = obama[keep_rows, keep_cols, ]

trans_seq = seq(0, 1, by = 1)
imgs = lapply(trans_seq,
              function(x){
                start * (1 - x) + end * x
              })
fnames = file.path("pngs",
                   sprintf("trans_%02.2f.png", trans_seq))
mapply(function(img, fname){
  writePNG(img, target = fname)
}, imgs, fnames)
out_gif = output = file.path("pngs", "transition.gif")
# im.convert(fnames, out_gif, extra.opts = "-delay 1")
cmd = sprintf("convert -morph 20 %s %s %s %s %s", fnames[1], fnames[1],
              fnames[2], fnames[2], out_gif)
system(cmd)

# remove NAs
# out_nonlin = out_nonlin_withna
# out_nonlin[ is.nan(out_nonlin)] = 0
# out_nonlin[ out_nonlin < 0] = 0
# out_nonlin[ out_nonlin > 1] = 1
# writePNG(out_nonlin, 
#     target = make_png_name(trump_stub, "bw_nonlin"))

ratio = obama_orig / reg_aff
writePNG(ratio,
         target = make_png_name(obama_stub,
            "compared_to_trump_ratio"))

diff = obama_orig - reg_aff
writePNG(diff,
         target = make_png_name(obama_stub,
            "compared_to_trump_diff"))

d = dim(diff)
if (all(d == c(631, 595))) {
    inds = 120:420
    mid_inds = 240:370
} else {
  if (all(d == c(744, 992))) {
    inds = 120:d[1]
    mid_inds = 200:650
  } else {
    stop("don't know inds yet")
  }
}
# cut off bottom
obama_cut = obama_orig
obama_cut = obama_cut[ inds, ]
# display(obama_cut)

trump_cut = reg_aff
trump_cut = trump_cut[ inds, ]
# display(trump_cut)


# display(trump_cut[,mid_inds])

rat_cut = ratio[inds, mid_inds]
writePNG(rat_cut,
    target = make_png_name(obama_stub,
            "compared_to_trump_ratio_cut"))

diff_cut = diff[inds, mid_inds]
writePNG(diff_cut,
    target = make_png_name(obama_stub,
            "compared_to_trump_diff_cut"))

obama_darker = obama_bw > reg_aff
class(obama_darker) = "numeric"
writePNG(obama_darker,
    target = make_png_name(obama_stub,
            "obama_darker"))

tr_darker = obama_bw < reg_aff
class(tr_darker) = "numeric"
writePNG(tr_darker,
    target = make_png_name(obama_stub,
            "trump_darker"))

trump_cut = reg_aff[inds, mid_inds]
writePNG(trump_cut,
    target = make_png_name(trump_stub,
            "cut"))

obama_cut = obama_bw[inds, mid_inds]
writePNG(obama_cut,
    target = make_png_name(obama_stub,
            "cut"))



