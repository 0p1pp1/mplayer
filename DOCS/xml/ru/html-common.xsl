<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:param name="chunker.output.encoding" select="'KOI8-R'"/>
  <xsl:output encoding="KOI8-R"/>

  <!-- This forces Saxon to output chars from KOI8-R as is and not as something like &#444;
    WARNING: This will probably work with HTML output, and won't with output XML.
    Anyway, this file will generate only HTML :)
    -->
  <xsl:param name="saxon.character.representation" select="'native;decimal'"/>

</xsl:stylesheet>
