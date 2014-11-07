/**
 * Copyright (c) 2014 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.operation.annotated;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;

import org.junit.BeforeClass;
import org.junit.Test;
import org.ow2.petals.activitibpmn.ActivitiSuManagerTest;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.MultipleBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoWsdlBindingException;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

/**
 * Unit tests of {@link AnnotatedWsdlParser}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class AnnotatedWsdlParserTest {

    private final Logger logger = Logger.getLogger(AnnotatedWsdlParserTest.class.getName());

    private final AnnotatedWsdlParser parser = new AnnotatedWsdlParser(this.logger);

    @BeforeClass
    public static void setLogging() throws SecurityException, IOException {
        final InputStream inputStream = ActivitiSuManagerTest.class.getResourceAsStream("/logging.properties");
        assertNotNull("Logging configuration file not found", inputStream);
        try {
            LogManager.getLogManager().readConfiguration(inputStream);
        } finally {
            inputStream.close();
        }
    }

    /**
     * <p>
     * Check the parser against a WSDL that does not contain binding
     * </p>
     * <p>
     * Expected results: An error occurs about missing binding
     * </p>
     */
    @Test
    public void parse_WsdlWithoutBinding() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/abstract-import.wsdl");
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document docWsdl;
        try {
            docWsdl = docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }

        assertEquals(0, this.parser.parse(docWsdl).size());
        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(2, encounteredErrors.size());
        boolean noWsdlBindingExceptionCounter = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoWsdlBindingException) {
                noWsdlBindingExceptionCounter = true;
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(noWsdlBindingExceptionCounter);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL that does not contain BPMN annotation
     * </p>
     * <p>
     * Expected results: An error occurs about missing annotations
     * </p>
     */
    @Test
    public void parse_WsdlWithoutBpmnAnnotations() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/withoutBpmnAnnotation.wsdl");
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document docWsdl;
        try {
            docWsdl = docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }

        assertEquals(0, this.parser.parse(docWsdl).size());
        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(4, encounteredErrors.size());
        int multipleBpmnOperationDefinedExceptionCounter = 0;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoBpmnOperationDefinedException) {
                multipleBpmnOperationDefinedExceptionCounter++;
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertEquals(3, multipleBpmnOperationDefinedExceptionCounter);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL that contains an WSDL operation containing several BPMN operations
     * </p>
     * <p>
     * Expected results: An error occurs about multiple BPMN operations
     * </p>
     */
    @Test
    public void parse_WsdlWithWsdlOpWithMultipleBpmnOp() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/several-bpmn-op-in-one-wsdl-op.wsdl");
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document docWsdl;
        try {
            docWsdl = docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }

        assertEquals(0, this.parser.parse(docWsdl).size());
        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(2, encounteredErrors.size());
        boolean multipleBpmnOperationDefinedExceptionFound = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof MultipleBpmnOperationDefinedException) {
                multipleBpmnOperationDefinedExceptionFound = true;
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(multipleBpmnOperationDefinedExceptionFound);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a valid WSDL
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>No error occurs</li>
     * <li>The expected annotated operation are retrieved</li>
     * </ul>
     * </p>
     */
    @Test
    public void parse_WsdlValid() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream("parser/valid.wsdl");
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document docWsdl;
        try {
            docWsdl = docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }

        final List<AnnotatedOperation> annotatedOperations = this.parser.parse(docWsdl);
        assertEquals(0, this.parser.getEncounteredErrors().size());
        assertEquals(3, annotatedOperations.size());
        boolean op1_found = true;
        boolean op2_found = true;
        boolean op3_found = true;
        for (final AnnotatedOperation annotatedoperation : annotatedOperations) {
            if (annotatedoperation instanceof StartEventAnnotatedOperation
                    && annotatedoperation.getWsdlOperationName().equals("demanderConges")) {

            } else if (annotatedoperation instanceof CompleteUserTaskAnnotatedOperation
                    && annotatedoperation.getWsdlOperationName().equals("validerDemande")) {

            } else if (annotatedoperation instanceof CompleteUserTaskAnnotatedOperation
                    && annotatedoperation.getWsdlOperationName().equals("ajusterDemande")) {
            } else {
                fail("Unexpected annotated operation");
            }
        }
        assertTrue(op1_found);
        assertTrue(op2_found);
        assertTrue(op3_found);
    }

    /**
     * <p>
     * Check the parser against a valid WSDL containing imports
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>No error occurs</li>
     * <li>The expected annotated operation are retrieved</li>
     * </ul>
     * </p>
     */
    @Test
    public void parse_WsdlValidWithImports() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/valid-with-imports.wsdl");
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document docWsdl;
        try {
            docWsdl = docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }

        final List<AnnotatedOperation> annotatedOperations = this.parser.parse(docWsdl);
        assertEquals(0, this.parser.getEncounteredErrors().size());
        assertEquals(3, annotatedOperations.size());
        boolean op1_found = true;
        boolean op2_found = true;
        boolean op3_found = true;
        for (final AnnotatedOperation annotatedoperation : annotatedOperations) {
            if (annotatedoperation instanceof StartEventAnnotatedOperation
                    && annotatedoperation.getWsdlOperationName().equals("demanderConges")) {

            } else if (annotatedoperation instanceof CompleteUserTaskAnnotatedOperation
                    && annotatedoperation.getWsdlOperationName().equals("validerDemande")) {

            } else if (annotatedoperation instanceof CompleteUserTaskAnnotatedOperation
                    && annotatedoperation.getWsdlOperationName().equals("ajusterDemande")) {
            } else {
                fail("Unexpected annotated operation");
            }
        }
        assertTrue(op1_found);
        assertTrue(op2_found);
        assertTrue(op3_found);
    }

}
