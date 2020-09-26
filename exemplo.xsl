<?xml version="1.0" ?>
<HTML xmlns:xsl="http://www.w3.org/TR/WD-xsl">
<BODY>
 <TABLE BORDER="0" CELLPADDING="2">
                <tr>
                   <td><p align="right">Codigo</p></td>
                   <td>Descricao</td>
                   <td><p align="right">Preco</p></td>
                   <td><p align="right">Tipo</p></td>
                   <td><p align="right">Importado</p></td>
                   <td><p align="right">Garantia</p></td>
                   <td><p align="right">Duravel</p></td>
              </tr>
              <xsl:for-each select="exemplo/Data/Record">
                <TR>
                <TD>
                     <p align="right"><xsl:value-of select="Codigo"/></p>
                </TD>
                <TD>
                     <xsl:value-of select="Descricao"/>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Preco"/></p>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Tipo"/></p>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Importado"/></p>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Garantia"/></p>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Duravel"/></p>
                </TD>
                </TR>
             </xsl:for-each>
           </TABLE>
</BODY>
</HTML>
