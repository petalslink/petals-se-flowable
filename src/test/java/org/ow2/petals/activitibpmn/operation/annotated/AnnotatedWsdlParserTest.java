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
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.MultipleBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoProcessIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoWsdlBindingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UnsupportedBpmnActionTypeException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UserIdMappingExpressionException;
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
     * Expected results: An error occurs about missing annotations for each wsdl operations, and an error occurs about
     * no annotated operation found.
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

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the BPMN action type is unknown.
     * </p>
     * <p>
     * Expected results: An error occurs about the unknown BPMN action type.
     * </p>
     */
    @Test
    public void parse_WsdlWithUnknownBpmnAction() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/unknown-bpmn-action-type.wsdl");
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
        boolean unknownBpmnActionTypeExceptionFound = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof UnsupportedBpmnActionTypeException) {
                unknownBpmnActionTypeExceptionFound = true;
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unknownBpmnActionTypeExceptionFound);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the user identifier place holder is as following
     * for the BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>tag missing (ie. no XML tag user id),</li>
     * <li>placeholder missing (ie. no XML attribute IN msg),</li>
     * <li>empty (ie. the XML attribute IN msg is empty).</li>
     * </ul>
     * </p>
     * <p>
     * Expected results: An error occurs about a missing or empty user id placeholder for both BPMN actions, and an
     * error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithUserIdPlaceHolderMissing() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/missing-and-empty-user-id.wsdl");
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
        assertEquals(7, encounteredErrors.size());
        boolean missingTagUserIdMappingOp1 = false;
        boolean missingTagUserIdMappingOp2 = false;
        boolean missingAttrUserIdMappingOp1 = false;
        boolean missingAttrUserIdMappingOp2 = false;
        boolean emptyUserIdMappingOp1 = false;
        boolean emptyUserIdMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoUserIdMappingException) {
                if (((NoUserIdMappingException) exception).getWsdlOperationName().equals("demanderConges_missingTag")) {
                    missingTagUserIdMappingOp1 = true;
                } else if (((NoUserIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingTag")) {
                    missingTagUserIdMappingOp2 = true;
                } else if (((NoUserIdMappingException) exception).getWsdlOperationName().equals(
                        "demanderConges_missingAttr")) {
                    missingAttrUserIdMappingOp1 = true;
                } else if (((NoUserIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingAttr")) {
                    missingAttrUserIdMappingOp2 = true;
                } else if (((NoUserIdMappingException) exception).getWsdlOperationName().equals("demanderConges_empty")) {
                    emptyUserIdMappingOp1 = true;
                } else if (((NoUserIdMappingException) exception).getWsdlOperationName().equals("validerDemande_empty")) {
                    emptyUserIdMappingOp2 = true;
                } else {
                    fail("Unexpected operation: "
                            + ((InvalidAnnotationForOperationException) exception).getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(missingTagUserIdMappingOp1);
        assertTrue(missingTagUserIdMappingOp2);
        assertTrue(missingAttrUserIdMappingOp1);
        assertTrue(missingAttrUserIdMappingOp2);
        assertTrue(emptyUserIdMappingOp1);
        assertTrue(emptyUserIdMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the user identifier place holder is set to an
     * invalid XPATH expression for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the invalid expression of the user id placeholder for both BPMN actions,
     * and an error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithUserIdPlaceHolderInvalidExpr() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/invalid-user-id.wsdl");
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
        assertEquals(3, encounteredErrors.size());
        boolean invalidUserIdMappingOp1 = false;
        boolean invalidUserIdMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof UserIdMappingExpressionException) {
                if (((UserIdMappingExpressionException) exception).getWsdlOperationName().equals("demanderConges")) {
                    invalidUserIdMappingOp1 = true;
                } else if (((UserIdMappingExpressionException) exception).getWsdlOperationName().equals(
                        "validerDemande")) {
                    invalidUserIdMappingOp2 = true;
                } else {
                    fail("Unexpected operation: "
                            + ((InvalidAnnotationForOperationException) exception).getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(invalidUserIdMappingOp1);
        assertTrue(invalidUserIdMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the process instance identifier place holder is
     * as following for the BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>tag missing (ie. no XML tag process instance id),</li>
     * <li>for BPMN action 'startEvent':
     * <ul>
     * <li>placeholder missing (ie. no XML attribute OUT msg),</li>
     * <li>empty (ie. the XML attribute OUT msg is empty).</li>
     * </ul>
     * </li>
     * <li>for BPMN action 'userTask':
     * <ul>
     * <li>placeholder missing (ie. no XML attribute IN msg),</li>
     * <li>empty (ie. the XML attribute IN msg is empty).</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>an error occurs about a missing or empty process instance id placeholder for BPMN action 'userTask',</li>
     * <li>no error occurs for BPMN action 'startEvent'</li>
     * </p>
     */
    @Test
    public void parse_WsdlWithProcessInstanceIdPlaceHolderMissing() throws SAXException, IOException {

        final InputStream is = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("parser/missing-and-empty-process-instance-id.wsdl");
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document docWsdl;
        try {
            docWsdl = docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }

        // The process instance id is not mandatory in output for BPMN action 'startEvent'
        assertEquals(3, this.parser.parse(docWsdl).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean missingTagProcessInstanceIdMappingIn = false;
        boolean missingAttrProcessInstanceIdMappingIn = false;
        boolean emptyProcessInstanceIdMappingIn = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoProcessIdMappingException) {
                if (((NoProcessIdMappingException) exception).getWsdlOperationName()
                        .equals(
                        "validerDemande_missingTag")) {
                    missingTagProcessInstanceIdMappingIn = true;
                } else if (((NoProcessIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingAttr")) {
                    missingAttrProcessInstanceIdMappingIn = true;
                } else if (((NoProcessIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_empty")) {
                    emptyProcessInstanceIdMappingIn = true;
                } else {
                    fail("Unexpected operation: " + ((NoUserIdMappingException) exception).getWsdlOperationName());
                }
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(missingTagProcessInstanceIdMappingIn);
        assertTrue(missingAttrProcessInstanceIdMappingIn);
        assertTrue(emptyProcessInstanceIdMappingIn);
    }

}
