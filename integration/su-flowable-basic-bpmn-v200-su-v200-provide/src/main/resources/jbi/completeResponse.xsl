<?xml version="1.0" encoding="UTF-8"?>
<!--
 Copyright (c) 2020-2022 Linagora
 
 This program/library is free software: you can redistribute it and/or modify
 it under the terms of the New BSD License (3-clause license).

 This program/library is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the New BSD License (3-clause license)
 for more details.

 You should have received a copy of the New BSD License (3-clause license)
 along with this program/library; If not, see http://directory.fsf.org/wiki/License:BSD_3Clause/
 for the New BSD License (3-clause license).
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:se-out-special-params="http://petals.ow2.org/se/flowable/output-params/1.0/special"
    xmlns:se-out-process-params="http://petals.ow2.org/se/flowable/output-params/1.0/process-instance">

   <xsl:output method="xml" encoding="UTF-8" indent="yes" omit-xml-declaration="no"/>
   
   <!-- The process instance id retrieve from the BPMN engine -->
   <xsl:param name="se-out-special-params:processInstanceId" />

   <xsl:template match="/">
      <xsl:element name="completeResponse" namespace="http://petals.ow2.org/integration/tests/se-flowable/basic/services/v2">
         <xsl:element name="process-instance-id">
            <xsl:value-of select="$se-out-special-params:processInstanceId"/>
         </xsl:element>
      </xsl:element>
   </xsl:template>

</xsl:stylesheet>
