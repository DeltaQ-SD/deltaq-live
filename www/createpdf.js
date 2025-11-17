function createAndDownloadPDF() {
  const compress = true,
      pagewidth = parseFloat(document.getElementById("out-diagram").children[0].width.baseVal.value),
      pageheight = parseFloat(document.getElementById("out-diagram").children[0].height.baseVal.value);
  const options = {
      useCSS: false,
      assumePt: true,
      preserveAspectRatio: false,
      width: pagewidth,
      height: pageheight
  };
  let doc = new PDFDocument({compress: compress, size: [pagewidth, pageheight]}),
      out = document.getElementById('out-diagram');
  SVGtoPDF(doc, out.innerHTML, 0, 0, options);
  let stream = doc.pipe(blobStream());
  stream.on('finish', function() {
    const blob = stream.toBlob('application/pdf');
    const url = URL.createObjectURL(blob);
    const a = document.getElementById('pdf-file');
    a.href = url;
    a.click();
  });
  doc.end();
}
