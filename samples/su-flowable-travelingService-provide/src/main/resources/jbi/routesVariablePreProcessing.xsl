<?xml version="1.0" encoding="UTF-8"?>
<!--
 Copyright (c) 2019-2022 Linagora
 
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
<xsl:stylesheet version="1.0"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:traveling="http://petals.ow2.org/samples/se-flowable/traveling/travelingService">

   <xsl:output method="xml" encoding="UTF-8" indent="yes" omit-xml-declaration="no" />

   <xsl:template match="/">
      <xsl:apply-templates select="routes" />
   </xsl:template>

   <xsl:template match="routes">
      <xsl:element name="routes">
         <xsl:processing-instruction name="xml-multiple">trains</xsl:processing-instruction>
         <xsl:apply-templates select="train" />
         <xsl:processing-instruction name="xml-multiple">planes</xsl:processing-instruction>
         <xsl:apply-templates select="plane" />
         <xsl:processing-instruction name="xml-multiple">hotels</xsl:processing-instruction>
         <xsl:apply-templates select="hotel" />
      </xsl:element>
   </xsl:template>

   <xsl:template match="train">
      <xsl:element name="trains">
         <xsl:element name="startFrom">
            <xsl:value-of select="start-from" />
         </xsl:element>
         <xsl:element name="endTo">
            <xsl:value-of select="end-to" />
         </xsl:element>
         <xsl:element name="date">
            <xsl:value-of select="date" />
         </xsl:element>
      </xsl:element>
   </xsl:template>

   <xsl:template match="plane">
      <xsl:element name="planes">
         <xsl:element name="startFrom">
            <xsl:value-of select="start-from" />
         </xsl:element>
         <xsl:element name="endTo">
            <xsl:value-of select="end-to" />
         </xsl:element>
         <xsl:element name="date">
            <xsl:value-of select="date" />
         </xsl:element>
      </xsl:element>
   </xsl:template>

   <xsl:template match="hotel">
      <xsl:element name="hotels">
         <xsl:element name="address">
            <xsl:value-of select="address" />
         </xsl:element>
         <xsl:element name="startDate">
            <xsl:value-of select="start-date" />
         </xsl:element>
         <xsl:element name="endDate">
            <xsl:value-of select="end-date" />
         </xsl:element>
      </xsl:element>
   </xsl:template>

</xsl:stylesheet>