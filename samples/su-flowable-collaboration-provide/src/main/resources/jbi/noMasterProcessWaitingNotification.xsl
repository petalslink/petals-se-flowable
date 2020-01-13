<?xml version="1.0" encoding="UTF-8"?>
<!--
 Copyright (c) 2018-2020 Linagora
 
 This program/library is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, either version 2.1 of the License, or (at your
 option) any later version.
 
 This program/library is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 for more details.
 
 You should have received a copy of the GNU Lesser General Public License
 along with this program/library; If not, see http://www.gnu.org/licenses/
 for the GNU Lesser General Public License version 2.1.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:bpmn-fault="http://petals.ow2.org/se/flowable/faults/1.0">

   <xsl:output method="xml" encoding="UTF-8" indent="yes" omit-xml-declaration="no"/>
   
   <xsl:param name="bpmn-fault:processInstanceId" />
   <xsl:param name="bpmn-fault:messageName" />

   <xsl:template match="/">
      <xsl:element name="noMasterProcessWaitingNotification" namespace="http://petals.ow2.org/samples/se-flowable/collaboration/services"/>
   </xsl:template>

</xsl:stylesheet>
