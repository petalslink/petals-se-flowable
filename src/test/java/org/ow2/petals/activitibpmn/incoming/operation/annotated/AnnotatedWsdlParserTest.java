/**
 * Copyright (c) 2015 Linagora
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
package org.ow2.petals.activitibpmn.incoming.operation.annotated;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.converter.util.InputStreamProvider;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.engine.impl.util.io.InputStreamSource;
import org.junit.BeforeClass;
import org.junit.Test;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.ActionIdNotFoundInModelException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.DuplicatedFaultMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.DuplicatedOutputMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.DuplicatedVariableException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.FaultXslNotFoundException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidFaultXslException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidOutputXslException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.MultipleBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoActionIdMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoBpmnOperationException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoFaultMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoFaultNameMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoOutputMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoProcessDefinitionIdMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoVariableMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoWsdlBindingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.OutputXslNotFoundException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.ProcessDefinitionIdDuplicatedInModelException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.ProcessDefinitionIdNotFoundInModelException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.ProcessInstanceIdMappingExpressionException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.RequiredVariableMissingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.UnsupportedActionException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.UserIdMappingExpressionException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.VariableMappingExpressionException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.VariableNameMissingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.VariableNotFoundInModelException;
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

    private static String SU_ROOT_PATH;

    private final Logger logger = Logger.getLogger(AnnotatedWsdlParserTest.class.getName());

    private final AnnotatedWsdlParser parser = new AnnotatedWsdlParser(this.logger);

    @BeforeClass
    public static void setLogging() throws SecurityException, IOException {
        final InputStream inputStream = AnnotatedWsdlParserTest.class.getResourceAsStream("/logging.properties");
        assertNotNull("Logging configuration file not found", inputStream);
        try {
            LogManager.getLogManager().readConfiguration(inputStream);
        } finally {
            inputStream.close();
        }
    }

    @BeforeClass
    public static void setSuRootPath() throws URISyntaxException {
        final URL urlValidXsl = AnnotatedWsdlParserTest.class.getResource("/parser/xsl/valid.xsl");
        assertNotNull("Valid xsl resource not found", urlValidXsl);
        final File validXsl = new File(urlValidXsl.toURI());
        SU_ROOT_PATH = validXsl.getParentFile().getAbsolutePath();
    }

    /**
     * Read a WSDL as {@link Document} from a resource file
     * 
     * @param resourceName
     *            Name of the resource file
     * @return The WSDL as {@link Document}
     */
    private Document readWsdlDocument(final String resourceName) throws SAXException, IOException {
        final InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName);
        assertNotNull("WSDL not found", is);
        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        try {
            return docBuilder.parse(is);
        } finally {
            DocumentBuilders.releaseDocumentBuilder(docBuilder);
        }
    }

    /**
     * Read BPMN model from a resource file
     * 
     * @param resourceName
     *            Name of the resource file
     * @return The BPMN model
     */
    private BpmnModel readBpmnModel(final String resourceName) throws SAXException, IOException {
        final InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName);
        assertNotNull("Process definition file not found", is);
        try {
            final InputStreamProvider bpmnInputStreamSource = new InputStreamSource(is);
            return new BpmnXMLConverter().convertToBpmnModel(bpmnInputStreamSource, false, false);
        } finally {
            try {
                is.close();
            } catch (final IOException e) {
                this.logger.log(Level.WARNING, "Unable to close BPMN definition file ''. Error skiped !", e);
            }
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

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/abstract-import.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());
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

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/withoutBpmnAnnotation.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());
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

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/several-bpmn-op-in-one-wsdl-op.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());
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

        final List<AnnotatedOperation> annotatedOperations = this.parser.parse(
                this.readWsdlDocument("parser/valid.wsdl"),
                Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH);
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

        final List<AnnotatedOperation> annotatedOperations = this.parser.parse(
                this.readWsdlDocument("parser/valid-with-imports.wsdl"),
                Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH);
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

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/unknown-bpmn-action-type.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(2, encounteredErrors.size());
        boolean unknownBpmnActionTypeExceptionFound = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof UnsupportedActionException) {
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

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-user-id.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(7, encounteredErrors.size());
        boolean missingTagUserIdMappingOp1 = false;
        boolean missingTagUserIdMappingOp2 = false;
        boolean tagNoSetUserIdMappingOp1 = false;
        boolean tagNoSetUserIdMappingOp2 = false;
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
                        "demanderConges_missingValue")) {
                    tagNoSetUserIdMappingOp1 = true;
                } else if (((NoUserIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingValue")) {
                    tagNoSetUserIdMappingOp2 = true;
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
        assertTrue(tagNoSetUserIdMappingOp1);
        assertTrue(tagNoSetUserIdMappingOp2);
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

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/invalid-user-id.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

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

        // The process instance id is not mandatory in output for BPMN action 'startEvent'
        assertEquals(
                3,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-process-instance-id.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH)
                .size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean missingTagProcessInstanceIdMappingIn = false;
        boolean tagNoSetProcessInstanceIdMappingIn = false;
        boolean emptyProcessInstanceIdMappingIn = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoProcessInstanceIdMappingException) {
                if (((NoProcessInstanceIdMappingException) exception).getWsdlOperationName()
                        .equals(
                        "validerDemande_missingTag")) {
                    missingTagProcessInstanceIdMappingIn = true;
                } else if (((NoProcessInstanceIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingValue")) {
                    tagNoSetProcessInstanceIdMappingIn = true;
                } else if (((NoProcessInstanceIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_empty")) {
                    emptyProcessInstanceIdMappingIn = true;
                } else {
                    fail("Unexpected operation: "
                            + ((NoProcessInstanceIdMappingException) exception).getWsdlOperationName());
                }
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(missingTagProcessInstanceIdMappingIn);
        assertTrue(tagNoSetProcessInstanceIdMappingIn);
        assertTrue(emptyProcessInstanceIdMappingIn);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the process instance identifier place holder is
     * set to an invalid XPATH expression for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the invalid expression of the process instance id placeholder for BPMN
     * action 'userTask' only because the process instance identifier is unneeded for BPMN action 'startEvent'.
     * </p>
     */
    @Test
    public void parse_WsdlWithProcessInstanceIdPlaceHolderInvalidExpr() throws SAXException, IOException {

        assertEquals(
                1,
                this.parser.parse(this.readWsdlDocument("parser/invalid-process-instance-id.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(1, encounteredErrors.size());
        final InvalidAnnotationException exception = encounteredErrors.get(0);
        assertTrue(exception instanceof ProcessInstanceIdMappingExpressionException);
        assertEquals("validerDemande", ((ProcessInstanceIdMappingExpressionException) exception).getWsdlOperationName());
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the process definition identifier is as following
     * for the BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>attribute missing (ie. no attribute in the XML tag 'operation'),</li>
     * <li>empty attribute(ie. the XML attribute is empty).</li>
     * </ul>
     * </p>
     * <p>
     * Expected results: An error occurs about a missing or empty process definition id for both BPMN actions, and an
     * error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithProcessDefinitionIdMissing() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-process-definition-id.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH)
                .size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(5, encounteredErrors.size());
        boolean missingAttrMappingOp1 = false;
        boolean missingAttrMappingOp2 = false;
        boolean emptyAttrMappingOp1 = false;
        boolean emptyAttrMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoProcessDefinitionIdMappingException) {
                if (((NoProcessDefinitionIdMappingException) exception).getWsdlOperationName().equals(
                        "demanderConges_missingAttr")) {
                    missingAttrMappingOp1 = true;
                } else if (((NoProcessDefinitionIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingAttr")) {
                    missingAttrMappingOp2 = true;
                } else if (((NoProcessDefinitionIdMappingException) exception).getWsdlOperationName().equals(
                        "demanderConges_emptyAttr")) {
                    emptyAttrMappingOp1 = true;
                } else if (((NoProcessDefinitionIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_emptyAttr")) {
                    emptyAttrMappingOp2 = true;
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
        assertTrue(missingAttrMappingOp1);
        assertTrue(missingAttrMappingOp2);
        assertTrue(emptyAttrMappingOp1);
        assertTrue(emptyAttrMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the action identifier is as following for the
     * BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>attribute missing (ie. no attribute in the XML tag 'operation'),</li>
     * <li>empty attribute(ie. the XML attribute is empty).</li>
     * </ul>
     * </p>
     * <p>
     * Expected results: An error occurs about a missing or empty action id for both BPMN actions, and an error occurs
     * about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithActionIdMissing() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-action-id.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(5, encounteredErrors.size());
        boolean missingAttrMappingOp1 = false;
        boolean missingAttrMappingOp2 = false;
        boolean emptyAttrMappingOp1 = false;
        boolean emptyAttrMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoActionIdMappingException) {
                if (((NoActionIdMappingException) exception).getWsdlOperationName()
                        .equals("demanderConges_missingAttr")) {
                    missingAttrMappingOp1 = true;
                } else if (((NoActionIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_missingAttr")) {
                    missingAttrMappingOp2 = true;
                } else if (((NoActionIdMappingException) exception).getWsdlOperationName().equals(
                        "demanderConges_emptyAttr")) {
                    emptyAttrMappingOp1 = true;
                } else if (((NoActionIdMappingException) exception).getWsdlOperationName().equals(
                        "validerDemande_emptyAttr")) {
                    emptyAttrMappingOp2 = true;
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
        assertTrue(missingAttrMappingOp1);
        assertTrue(missingAttrMappingOp2);
        assertTrue(emptyAttrMappingOp1);
        assertTrue(emptyAttrMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the variable place holder is set to an invalid
     * XPATH expression for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the invalid expression of the variable placeholder for both BPMN actions,
     * and an error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithVariablePlaceHolderInvalidExpr() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/invalid-variable-placeholder.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean invalidVariableMappingOp1 = false;
        boolean invalidVariableMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof VariableMappingExpressionException) {
                final VariableMappingExpressionException expectedException = (VariableMappingExpressionException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    invalidVariableMappingOp1 = true;
                    assertEquals("numberOfDays", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    invalidVariableMappingOp2 = true;
                    assertEquals("vacationApproved", expectedException.getVariableName());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(invalidVariableMappingOp1);
        assertTrue(invalidVariableMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the variable place holder is as following for the
     * BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>variable tag missing (ie. no XML tag user id),</li>
     * <li>variable name is missing,</li>
     * <li>variable name is empty,</li>
     * <li>variable placeholder is missing,</li>
     * <li>variable placeholder is empty.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results: An error occurs about a missing or empty variable placeholder for both BPMN actions, and an
     * error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithVariablePlaceHolderMissing() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-variable-expression.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH)
                .size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(11, encounteredErrors.size());
        boolean missingTagVariableMappingOp1 = false;
        boolean missingTagVariableMappingOp2 = false;
        boolean missingVariableNameMappingOp1 = false;
        boolean missingVariableNameMappingOp2 = false;
        boolean emptyVariableNameMappingOp1 = false;
        boolean emptyVariableNameMappingOp2 = false;
        boolean missingVariablePlaceholderMappingOp1 = false;
        boolean missingVariablePlaceholderMappingOp2 = false;
        boolean emptyVariablePlaceholderMappingOp1 = false;
        boolean emptyVariablePlaceholderMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof RequiredVariableMissingException) {
                final RequiredVariableMissingException expectedException = (RequiredVariableMissingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges_missingVariableTag")) {
                    missingTagVariableMappingOp1 = true;
                    assertEquals("numberOfDays", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingVariableTag")) {
                    missingTagVariableMappingOp2 = true;
                    assertEquals("vacationApproved", expectedException.getVariableName());
                } else {
                    fail("Unexpected operation: "
                            + ((InvalidAnnotationForOperationException) exception).getWsdlOperationName());
                }
            } else if (exception instanceof VariableNameMissingException) {
                final VariableNameMissingException expectedException = (VariableNameMissingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges_missingVariableName")) {
                    missingVariableNameMappingOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingVariableName")) {
                    missingVariableNameMappingOp2 = true;
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_emptyVariableName")) {
                    emptyVariableNameMappingOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_emptyVariableName")) {
                    emptyVariableNameMappingOp2 = true;
                } else {
                    fail("Unexpected operation: "
                            + ((InvalidAnnotationForOperationException) exception).getWsdlOperationName());
                }
            } else if (exception instanceof NoVariableMappingException) {
                final NoVariableMappingException expectedException = (NoVariableMappingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges_noVariableContent")) {
                    missingVariablePlaceholderMappingOp1 = true;
                    assertEquals("numberOfDays", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_noVariableContent")) {
                    missingVariablePlaceholderMappingOp2 = true;
                    assertEquals("vacationApproved", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_emptyVariableContent")) {
                    emptyVariablePlaceholderMappingOp1 = true;
                    assertEquals("numberOfDays", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_emptyVariableContent")) {
                    emptyVariablePlaceholderMappingOp2 = true;
                    assertEquals("vacationApproved", expectedException.getVariableName());
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
        assertTrue(missingTagVariableMappingOp1);
        assertTrue(missingTagVariableMappingOp2);
        assertTrue(missingVariableNameMappingOp1);
        assertTrue(missingVariableNameMappingOp2);
        assertTrue(emptyVariableNameMappingOp1);
        assertTrue(emptyVariableNameMappingOp2);
        assertTrue(missingVariablePlaceholderMappingOp1);
        assertTrue(missingVariablePlaceholderMappingOp2);
        assertTrue(emptyVariablePlaceholderMappingOp1);
        assertTrue(emptyVariablePlaceholderMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the process definition identifier does not exist
     * into the BPMN model for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the unexisting process definition identifier for both BPMN actions
     * 'startEvent' and 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithUnexistingProcessDefinitionId() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/process-definition-id-not-found-in-model.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean unexistingProcessDefinitionIdOp1 = false;
        boolean unexistingProcessDefinitionIdOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof ProcessDefinitionIdNotFoundInModelException) {
                final ProcessDefinitionIdNotFoundInModelException expectedException = (ProcessDefinitionIdNotFoundInModelException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    unexistingProcessDefinitionIdOp1 = true;
                    assertEquals("unexistingProcessDefinitionId", expectedException.getProcessDefinitionId());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    unexistingProcessDefinitionIdOp2 = true;
                    assertEquals("unexistingProcessDefinitionId", expectedException.getProcessDefinitionId());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unexistingProcessDefinitionIdOp1);
        assertTrue(unexistingProcessDefinitionIdOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the process definition identifier is declared in
     * several BPMN models for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the duplicated process definition identifiers for both BPMN actions
     * 'startEvent' and 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithDuplicatedProcessDefinitionId() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(
                        this.readWsdlDocument("parser/valid.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml"),
                                this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(4, encounteredErrors.size());
        boolean unexistingProcessDefinitionIdOp1 = false;
        boolean unexistingProcessDefinitionIdOp2 = false;
        boolean unexistingProcessDefinitionIdOp3 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof ProcessDefinitionIdDuplicatedInModelException) {
                final ProcessDefinitionIdDuplicatedInModelException expectedException = (ProcessDefinitionIdDuplicatedInModelException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    unexistingProcessDefinitionIdOp1 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    unexistingProcessDefinitionIdOp2 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                } else if (expectedException.getWsdlOperationName().equals("ajusterDemande")) {
                    unexistingProcessDefinitionIdOp3 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unexistingProcessDefinitionIdOp1);
        assertTrue(unexistingProcessDefinitionIdOp2);
        assertTrue(unexistingProcessDefinitionIdOp3);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the action identifier does not exist into the
     * BPMN model for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the unexisting action identifier for both BPMN actions 'startEvent' and
     * 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithUnexistingActionId() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/action-id-not-found-in-model.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean unexistingActionIdOp1 = false;
        boolean unexistingActionIdOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof ActionIdNotFoundInModelException) {
                final ActionIdNotFoundInModelException expectedException = (ActionIdNotFoundInModelException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    unexistingActionIdOp1 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                    assertEquals("unexisting-request", expectedException.getActionId());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    unexistingActionIdOp2 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                    assertEquals("unexisting-handleRequest", expectedException.getActionId());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unexistingActionIdOp1);
        assertTrue(unexistingActionIdOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but a variable declaration is duplicated for the BPMN
     * actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the variable duplication for both BPMN actions, and an error occurs about
     * no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithDuplicatedVariables() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/duplicated-variables.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean duplicatedVariableMappingOp1 = false;
        boolean duplicatedVariableMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof DuplicatedVariableException) {
                final DuplicatedVariableException expectedException = ((DuplicatedVariableException) exception);
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    duplicatedVariableMappingOp1 = true;
                    assertEquals("numberOfDays", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    duplicatedVariableMappingOp2 = true;
                    assertEquals("vacationApproved", expectedException.getVariableName());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(duplicatedVariableMappingOp1);
        assertTrue(duplicatedVariableMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but a variable declared does not exist into the BPMN
     * model for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the unexisting variable for both BPMN actions 'startEvent' and
     * 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithUnexistingVariable() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/variable-not-found-in-model.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean unexistingActionIdOp1 = false;
        boolean unexistingActionIdOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof VariableNotFoundInModelException) {
                final VariableNotFoundInModelException expectedException = (VariableNotFoundInModelException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    unexistingActionIdOp1 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                    assertEquals("unexisting-variable-1", expectedException.getVariableName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    unexistingActionIdOp2 = true;
                    assertEquals("vacationRequest", expectedException.getProcessDefinitionId());
                    assertEquals("unexisting-variable-2", expectedException.getVariableName());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unexistingActionIdOp1);
        assertTrue(unexistingActionIdOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the output XSLT style-sheet is set to an invalid
     * XSL style-sheet for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the invalid output XSLT style-sheet for both BPMN actions, and an error
     * occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithOutputXslInvalidExpr() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/invalid-output-xsl.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean invalidOutputXsltMappingOp1 = false;
        boolean invalidOutputXsltMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof InvalidOutputXslException) {
                final InvalidOutputXslException expectedException = (InvalidOutputXslException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    invalidOutputXsltMappingOp1 = true;
                    assertEquals("invalid.xsl", expectedException.getXslFileName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    invalidOutputXsltMappingOp2 = true;
                    assertEquals("invalid.xsl", expectedException.getXslFileName());
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
        assertTrue(invalidOutputXsltMappingOp1);
        assertTrue(invalidOutputXsltMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the output XSLT style-sheet does not exist in the
     * service unit for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the unexisting output XSLT style-sheet for both BPMN actions 'startEvent'
     * and 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithUnexistingOutputXslId() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/output-xsl-not-found.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean unexistingOutputXslOp1 = false;
        boolean unexistingOutputXslOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof OutputXslNotFoundException) {
                final OutputXslNotFoundException expectedException = (OutputXslNotFoundException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    unexistingOutputXslOp1 = true;
                    assertEquals("unexisting.xsl", expectedException.getXslFileName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    unexistingOutputXslOp2 = true;
                    assertEquals("unexisting.xsl", expectedException.getXslFileName());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unexistingOutputXslOp1);
        assertTrue(unexistingOutputXslOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the output XSLT style-sheet is as following for
     * the BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>tag missing (ie. no XML tag about the output XSLT style-sheet),</li>
     * <li>value missing (ie. the content of the XML tag about the output XSLT style-sheet is missing),</li>
     * <li>empty (ie. the content of XML tag about the output XSLT style-sheet is empty).</li>
     * </ul>
     * </p>
     * <p>
     * Expected results: An error occurs about a missing or empty output XSLT style-sheet for both BPMN actions, and an
     * error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithOutputXslMissing() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-output-xsl.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(7, encounteredErrors.size());
        boolean missingTagOutputXslOp1 = false;
        boolean missingTagOutputXslOp2 = false;
        boolean missingTagValueOutputXslOp1 = false;
        boolean missingTagValueOutputXslOp2 = false;
        boolean emptyOutputXslOp1 = false;
        boolean emptyOutputXslOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoOutputMappingException) {
                final NoOutputMappingException expectedException = (NoOutputMappingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges_missingTag")) {
                    missingTagOutputXslOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingTag")) {
                    missingTagOutputXslOp2 = true;
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_missingValue")) {
                    missingTagValueOutputXslOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingValue")) {
                    missingTagValueOutputXslOp2 = true;
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_empty")) {
                    emptyOutputXslOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_empty")) {
                    emptyOutputXslOp2 = true;
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
        assertTrue(missingTagOutputXslOp1);
        assertTrue(missingTagOutputXslOp2);
        assertTrue(missingTagValueOutputXslOp1);
        assertTrue(missingTagValueOutputXslOp2);
        assertTrue(emptyOutputXslOp1);
        assertTrue(emptyOutputXslOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but the output XSLT style-sheet is declared more than
     * once for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the duplicated output XSLT style-sheets for both BPMN actions
     * 'startEvent' and 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithDuplicatedOutputXsl() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/duplicated-output-xsl.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean duplicatedOutputXslOp1 = false;
        boolean duplicatedOutputXslOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof DuplicatedOutputMappingException) {
                final DuplicatedOutputMappingException expectedException = (DuplicatedOutputMappingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    duplicatedOutputXslOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    duplicatedOutputXslOp2 = true;
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(duplicatedOutputXslOp1);
        assertTrue(duplicatedOutputXslOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but a fault XSLT style-sheet is as following for the
     * BPMN actions 'userTask' and 'startEvent':
     * <ul>
     * <li>tag missing (ie. no XML tag about the fault XSLT style-sheet),</li>
     * <li>the mapped exception attribute is missing,</li>
     * <li>the mapped exception attribute is empty,</li>
     * <li>the value is missing (ie. the content of the XML tag about the fault XSLT style-sheet is missing),</li>
     * <li>the value is empty (ie. the content of XML tag about the fault XSLT style-sheet is empty).</li>
     * </ul>
     * </p>
     * <p>
     * Expected results: An error occurs about a missing or empty fault XSLT style-sheet for both BPMN actions, and an
     * error occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithFaultXslMissing() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/missing-and-empty-fault-xsl.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(11, encounteredErrors.size());
        boolean missingTagFaultXslOp1 = false;
        boolean missingTagFaultXslOp2 = false;
        boolean missingAttrFaultXslOp1 = false;
        boolean missingAttrFaultXslOp2 = false;
        boolean emptyAttrFaultXslOp1 = false;
        boolean emptyAttrFaultXslOp2 = false;
        boolean missingTagValueFaultXslOp1 = false;
        boolean missingTagValueFaultXslOp2 = false;
        boolean emptyFaultXslOp1 = false;
        boolean emptyFaultXslOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof NoFaultMappingException) {
                final NoFaultMappingException expectedException = (NoFaultMappingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges_missingTag")) {
                    missingTagFaultXslOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingTag")) {
                    missingTagFaultXslOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_missingValue")) {
                    missingTagValueFaultXslOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingValue")) {
                    missingTagValueFaultXslOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_empty")) {
                    emptyFaultXslOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_empty")) {
                    emptyFaultXslOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
                } else {
                    fail("Unexpected operation: "
                            + ((InvalidAnnotationForOperationException) exception).getWsdlOperationName());
                }
            } else if (exception instanceof NoFaultNameMappingException) {
                final NoFaultNameMappingException expectedException = (NoFaultNameMappingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges_missingAttr")) {
                    missingAttrFaultXslOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_missingAttr")) {
                    missingAttrFaultXslOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("demanderConges_emptyAttr")) {
                    emptyAttrFaultXslOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande_emptyAttr")) {
                    emptyAttrFaultXslOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
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
        assertTrue(missingTagFaultXslOp1);
        assertTrue(missingTagFaultXslOp2);
        assertTrue(missingAttrFaultXslOp1);
        assertTrue(missingAttrFaultXslOp2);
        assertTrue(emptyAttrFaultXslOp1);
        assertTrue(emptyAttrFaultXslOp2);
        assertTrue(missingTagValueFaultXslOp1);
        assertTrue(missingTagValueFaultXslOp2);
        assertTrue(emptyFaultXslOp1);
        assertTrue(emptyFaultXslOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but a fault XSLT style-sheet is declared more than
     * once for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the duplicated fault XSLT style-sheets for both BPMN actions 'startEvent'
     * and 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithDuplicatedFaultXsl() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/duplicated-fault-xsl.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean duplicatedFaultXslOp1 = false;
        boolean duplicatedFaultXslOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof DuplicatedFaultMappingException) {
                final DuplicatedFaultMappingException expectedException = (DuplicatedFaultMappingException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    duplicatedFaultXslOp1 = true;
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    duplicatedFaultXslOp2 = true;
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(duplicatedFaultXslOp1);
        assertTrue(duplicatedFaultXslOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but a fault XSLT style-sheet does not exist in the
     * service unit for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the unexisting fault XSLT style-sheet for both BPMN actions 'startEvent'
     * and 'userTask'.
     * </p>
     */
    @Test
    public void parse_WsdlWithUnexistingFaultXslId() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/fault-xsl-not-found.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean unexistingFaultXslOp1 = false;
        boolean unexistingFaultXslOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof FaultXslNotFoundException) {
                final FaultXslNotFoundException expectedException = (FaultXslNotFoundException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    unexistingFaultXslOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                    assertEquals("unexisting.xsl", expectedException.getXslFileName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    unexistingFaultXslOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
                    assertEquals("unexisting.xsl", expectedException.getXslFileName());
                } else {
                    fail("Unexpected operation: " + expectedException.getWsdlOperationName());
                }
            } else if (exception instanceof NoBpmnOperationException) {
                noBpmnOperationExceptionFound = true;
            } else {
                fail("Unexpected error: " + exception.getClass());
            }
        }
        assertTrue(unexistingFaultXslOp1);
        assertTrue(unexistingFaultXslOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }

    /**
     * <p>
     * Check the parser against a WSDL containing BPMN annotations but a fault XSLT style-sheet is set to an invalid XSL
     * style-sheet for the BPMN actions 'userTask' and 'startEvent'
     * </p>
     * <p>
     * Expected results: An error occurs about the invalid fault XSLT style-sheet for both BPMN actions, and an error
     * occurs about no valid annotated operation found.
     * </p>
     */
    @Test
    public void parse_WsdlWithFaultXslInvalidExpr() throws SAXException, IOException {

        assertEquals(
                0,
                this.parser.parse(this.readWsdlDocument("parser/invalid-fault-xsl.wsdl"),
                        Arrays.asList(this.readBpmnModel("parser/vacationRequest.bpmn20.xml")), SU_ROOT_PATH).size());

        final List<InvalidAnnotationException> encounteredErrors = this.parser.getEncounteredErrors();
        assertEquals(3, encounteredErrors.size());
        boolean invalidFaultXsltMappingOp1 = false;
        boolean invalidFaultXsltMappingOp2 = false;
        boolean noBpmnOperationExceptionFound = false;
        for (final InvalidAnnotationException exception : encounteredErrors) {
            if (exception instanceof InvalidFaultXslException) {
                final InvalidFaultXslException expectedException = (InvalidFaultXslException) exception;
                if (expectedException.getWsdlOperationName().equals("demanderConges")) {
                    invalidFaultXsltMappingOp1 = true;
                    assertEquals("demandeFault", expectedException.getWsdlFault());
                    assertEquals("invalid.xsl", expectedException.getXslFileName());
                } else if (expectedException.getWsdlOperationName().equals("validerDemande")) {
                    invalidFaultXsltMappingOp2 = true;
                    assertEquals("vacationFault", expectedException.getWsdlFault());
                    assertEquals("invalid.xsl", expectedException.getXslFileName());
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
        assertTrue(invalidFaultXsltMappingOp1);
        assertTrue(invalidFaultXsltMappingOp2);
        assertTrue(noBpmnOperationExceptionFound);
    }
}
