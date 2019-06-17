<?xml version="1.0" encoding="UTF-8"?>
<!--
 Copyright (c) 2014-2019 Linagora
 
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
    xmlns:se-out-special-params="http://petals.ow2.org/se/flowable/output-params/1.0/special"
    xmlns:se-out-process-params="http://petals.ow2.org/se/flowable/output-params/1.0/process-instance"
    xmlns:se-out-task-params="http://petals.ow2.org/se/flowable/output-params/1.0/task">

   <xsl:output method="xml" encoding="UTF-8" omit-xml-declaration="no"/>
   
   <!-- The process instance id retrieve from the BPMN engine -->
   <xsl:param name="se-out-special-params:processInstanceId" />
   <xsl:param name="se-out-special-params:userId" />
   <!-- Process instance variables -->
   <xsl:param name="se-out-process-params:employeeName" />
   <xsl:param name="se-out-process-params:numberOfDays" />
   <xsl:param name="se-out-process-params:startDate" />
   <xsl:param name="se-out-process-params:vacationApproved" />
   <xsl:param name="se-out-process-params:vacationMotivation" />

   <xsl:template match="/">
      <xsl:element name="ackResponse" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">process-instance-id</xsl:attribute>
            <xsl:value-of select="$se-out-special-params:processInstanceId"/>
         </xsl:element>
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">user-id</xsl:attribute>
            <xsl:value-of select="$se-out-special-params:userId"/>
         </xsl:element>
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">employeeName</xsl:attribute>
            <xsl:value-of select="$se-out-process-params:employeeName"/>
         </xsl:element>
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">numberOfDays</xsl:attribute>
            <xsl:value-of select="$se-out-process-params:numberOfDays"/>
         </xsl:element>
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">startDate</xsl:attribute>
            <xsl:value-of select="$se-out-process-params:startDate"/>
         </xsl:element>
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">vacationApproved</xsl:attribute>
            <xsl:value-of select="$se-out-process-params:vacationApproved"/>
         </xsl:element>
         <xsl:element name="xsl-parameter" namespace="http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService">
            <xsl:attribute name="name">vacationMotivation</xsl:attribute>
            <xsl:value-of select="$se-out-process-params:vacationMotivation"/>
         </xsl:element>
      </xsl:element>
   </xsl:template>

</xsl:stylesheet>
