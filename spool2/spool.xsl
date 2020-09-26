<?xsl version="1.0" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:template match="/">
<!-- COBOLware 5.0 -->
<HTML>
<BODY>
 <TABLE BORDER="0" CELLPADDING="2">
                <tr>
                   <td>Codigo</td>
                   <td><p align="right">Paginas</p></td>
                   <td><p align="right">Linhas</p></td>
                   <td><p align="right">Largura</p></td>
                   <td>Geracao</td>
                   <td>Hora</td>
                   <td>Proprietario</td>
                   <td>Titulo</td>
                   <td>Arquivo</td>
                   <td>Observacao</td>
                   <td>Controle</td>
              </tr>
              <xsl:for-each select="spool/Data/Record">
                <TR>
                <TD>
                     <xsl:value-of select="Codigo"/>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Paginas"/></p>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Linhas"/></p>
                </TD>
                <TD>
                     <p align="right"><xsl:value-of select="Largura"/></p>
                </TD>
                <TD>
                     <xsl:value-of select="Geracao"/>
                </TD>
                <TD>
                     <xsl:value-of select="Hora"/>
                </TD>
                <TD>
                     <xsl:value-of select="Proprietario"/>
                </TD>
                <TD>
                     <xsl:value-of select="Titulo"/>
                </TD>
                <TD>
                     <xsl:value-of select="Arquivo"/>
                </TD>
                <TD>
                     <xsl:value-of select="Observacao"/>
                </TD>
                <TD>
                     <xsl:value-of select="Controle"/>
                </TD>
                </TR>
             </xsl:for-each>
           </TABLE>
</BODY>
</HTML>
</xsl:template>
</xsl:stylesheet>
