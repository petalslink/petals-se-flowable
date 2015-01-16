<?xml version="1.0" encoding="UTF-8"?>
<!--
 Copyright (c) 2014-2015 Linagora
 
 This program/library is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, either version 2.1 of the License, or (at your
 option) any later version.
 
 This program/library is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 for more details.
 
 You should have received a copy of the GNU Lesser General Public License
 along with this program/library; If not, see <http://www.gnu.org/licenses/>
 for the GNU Lesser General Public License version 2.1.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bpmn-se-params="http://petals.ow2.org/se/bpmn/output-params/1.0">

   <xsl:output method="xml" encoding="UTF-8" omit-xml-declaration="no"/>
   
   <!-- The process instance id retrieve from the BPMN engine -->
   <xsl:param name="bpmn-se-params:processInstanceId" />

   <xsl:template match="/">
      <xsl:element name="numero" namespace="http://petals.ow2.org/se/Activitibpmn/1.0/su">
         <xsl:element name="numeroDde" namespace="http://petals.ow2.org/se/Activitibpmn/1.0/su">
            <xsl:value-of select="$bpmn-se-params:processInstanceId"/>
         </xsl:element>
      </xsl:element>
   </xsl:template>

</xsl:stylesheet>
