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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.activiti.bpmn.model.BpmnModel;
import org.ow2.petals.activitibpmn.operation.annotated.exception.DuplicatedOutputMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.DuplicatedVariableException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidOutputXslException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.MultipleBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoOutputMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoVariableMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoWsdlBindingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.OutputXslNotFoundException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.ProcessInstanceIdMappingExpressionException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UnsupportedActionException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UserIdMappingExpressionException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.VariableMappingExpressionException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.VariableNameMissingException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The parser to read anotation included into the WSDL
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class AnnotatedWsdlParser {

    private static final String SCHEMA_WSDL = "http://schemas.xmlsoap.org/wsdl/";

    private static final String SCHEMA_BPMN_ANNOTATIONS = "http://petals.ow2.org/se/bpmn/annotations/1.0";

    /**
     * Local part of the annotation tag associated to the definition of a BPMN operation
     */
    private static final String BPMN_ANNOTATION_OPERATION = "operation";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the process definition identifier
     */
    private static final String BPMN_ANNOTATION_PROCESS_DEFINITION_ID = "processDefinitionId";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the action to realize on the process
     * side
     */
    private static final String BPMN_ANNOTATION_ACTION = "action";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the identifier of the step on which
     * the action will be realized
     */
    private static final String BPMN_ANNOTATION_ACTION_ID = "actionId";

    /**
     * Local part of the annotation tag associated to the place holder containing the process instance identifier
     */
    private static final String BPMN_ANNOTATION_PROCESS_INSTANCE_ID_HOLDER = "processInstanceId";

    /**
     * Local part of the annotation tag associated to the place holder containing the user identifier
     */
    private static final String BPMN_ANNOTATION_USER_ID_HOLDER = "userId";

    /**
     * Local part of the annotation tag associated a to variable
     */
    private static final String BPMN_ANNOTATION_VARIABLE = "variable";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the variable name
     */
    private static final String BPMN_ANNOTATION_VARIABLE_NAME = "name";

    /**
     * Local part of the annotation tag associated a the output
     */
    private static final String BPMN_ANNOTATION_OUTPUT = "output";

    private final Logger logger;

    /**
     * List of errors encountered during a parsing. All errors are reseted when the parsing starts
     */
    private final List<InvalidAnnotationException> encounteredErrors = new ArrayList<InvalidAnnotationException>();

    private final ErrorListener transformerFactoryErrorListener;

    public AnnotatedWsdlParser(final Logger logger) {
        this.logger = logger;
        this.transformerFactoryErrorListener = new ErrorListener() {
            @Override
            public void warning(final TransformerException exception) throws TransformerException {
                AnnotatedWsdlParser.this.logger.warning(exception.getMessageAndLocation());
            }

            @Override
            public void fatalError(final TransformerException exception) throws TransformerException {
                AnnotatedWsdlParser.this.logger.severe(exception.getMessageAndLocation());
                throw exception;
            }

            @Override
            public void error(final TransformerException exception) throws TransformerException {
                AnnotatedWsdlParser.this.logger.severe(exception.getMessageAndLocation());
                throw exception;
            }
        };
    }

    /**
     * <p>
     * Parse the annotated WSDL.
     * </p>
     * <p>
     * If an annotated operation is invalid because of error using annotations, the operation is skipped.
     * </p>
     * 
     * @param annotatedWsdl
     *            The WSDL to parse containing BPMN annotation
     * @param bpmnModels
     *            BPMN models embedded into the service unit
     * @return For each operation of the WSDL, the associated annotated operation is returned.
     */
    public List<AnnotatedOperation> parse(final Document annotatedWsdl, final List<BpmnModel> bpmnModels,
            final String suRootPath) {

        this.encounteredErrors.clear();

        final List<AnnotatedOperation> annotatedOperations = new ArrayList<AnnotatedOperation>();

        annotatedWsdl.getDocumentElement().normalize();
        final XPathFactory xpathFactory = XPathFactory.newInstance();

        // Get the node "wsdl:binding"
        final NodeList bindings = annotatedWsdl.getElementsByTagNameNS(SCHEMA_WSDL, "binding");
        if (bindings.getLength() == 0) {
            final InvalidAnnotationException ex = new NoWsdlBindingException();
            this.encounteredErrors.add(ex);
            this.logger.log(Level.WARNING, ex.getMessage(), ex);
        } else {
            final Node binding = bindings.item(0);
            // Get the list of nodes "wsdl:operation"
            final NodeList wsdlOperations = ((Element) binding).getElementsByTagNameNS(SCHEMA_WSDL, "operation");

            for (int j = 0; j < wsdlOperations.getLength(); j++) {
                try {
                    final Node wsdlOperation = wsdlOperations.item(j);
                    // TODO: The namespace of the operation should be included in the operation name
                    final String wsdlOperationName = ((Element) wsdlOperation).getAttribute("name");

                    // Get the node "bpmn:operation"
                    final NodeList bpmnOperations = ((Element) wsdlOperation).getElementsByTagNameNS(
                            SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_OPERATION);
                    if (bpmnOperations.getLength() == 0) {
                        throw new NoBpmnOperationDefinedException(wsdlOperationName);
                    }
                    if (bpmnOperations.getLength() > 1) {
                        throw new MultipleBpmnOperationDefinedException(wsdlOperationName);
                    }
                    final Node bpmnOperation = bpmnOperations.item(0);

                    // get the process definition identifier
                    final String processDefinitionKey = ((Element) bpmnOperation)
                            .getAttribute(BPMN_ANNOTATION_PROCESS_DEFINITION_ID);

                    // get the action to do
                    final String action = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_ACTION);

                    // get the task identifier on which the action must be done
                    final String actionId = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_ACTION_ID);

                    // Get the node "bpmn:processId" and its message
                    final Node processInstanceId = ((Element) wsdlOperation).getElementsByTagNameNS(
                            SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_PROCESS_INSTANCE_ID_HOLDER).item(0);
                    final XPathExpression bpmnProcessInstanceId;
                    if (processInstanceId != null) {
                        final String xpathExpr = processInstanceId.getTextContent();
                        if (xpathExpr.trim().isEmpty()) {
                            throw new NoProcessInstanceIdMappingException(wsdlOperationName);
                        } else {
                            final XPath xpath = xpathFactory.newXPath();
                            try {
                                bpmnProcessInstanceId = xpath.compile(xpathExpr);
                            } catch (final XPathExpressionException e) {
                                throw new ProcessInstanceIdMappingExpressionException(wsdlOperationName, e);
                            }
                        }
                    } else {
                        bpmnProcessInstanceId = null;
                    }

                    // Get the node "bpmn:userId"
                    final Node userId = ((Element) wsdlOperation).getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS,
                            BPMN_ANNOTATION_USER_ID_HOLDER).item(0);
                    final XPathExpression bpmnUserId;
                    if (userId != null) {
                        final String xpathExpr = userId.getTextContent();
                        if (xpathExpr.trim().isEmpty()) {
                            throw new NoUserIdMappingException(wsdlOperationName);
                        } else {
                            final XPath xpath = xpathFactory.newXPath();
                            try {
                                bpmnUserId = xpath.compile(xpathExpr);
                            } catch (final XPathExpressionException e) {
                                throw new UserIdMappingExpressionException(wsdlOperationName, e);
                            }
                        }
                    } else {
                        bpmnUserId = null;
                    }

                    // Get the list of nodes "bpmn:variable"
                    final NodeList bpmnVariableList = ((Element) wsdlOperation).getElementsByTagNameNS(
                            SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_VARIABLE);
                    final Map<String, XPathExpression> bpmnOperationVariables = new HashMap<String, XPathExpression>();
                    for (int k = 0; k < bpmnVariableList.getLength(); k++) {
                        final Node bpmnVariable = bpmnVariableList.item(k);
                        // test name declaration of variable
                        final String bpmnVariableName = ((Element) bpmnVariable)
                                .getAttribute(BPMN_ANNOTATION_VARIABLE_NAME);
                        if (bpmnVariableName == null || bpmnVariableName.trim().isEmpty()) {
                            throw new VariableNameMissingException(wsdlOperationName);
                        }
                        // test unicity of declared variable for the operation
                        if (bpmnOperationVariables.containsKey(bpmnVariableName)) {
                            throw new DuplicatedVariableException(wsdlOperationName, bpmnVariableName);
                        }

                        final XPathExpression bpmnVariableXPath;
                        final String xpathExpr = bpmnVariable.getTextContent();
                        if (xpathExpr.trim().isEmpty()) {
                            throw new NoVariableMappingException(wsdlOperationName, bpmnVariableName);
                        } else {
                            final XPath xpath = xpathFactory.newXPath();
                            try {
                                bpmnVariableXPath = xpath.compile(xpathExpr);
                            } catch (final XPathExpressionException e) {
                                throw new VariableMappingExpressionException(wsdlOperationName, bpmnVariableName, e);
                            }
                        }

                        bpmnOperationVariables.put(bpmnVariableName, bpmnVariableXPath);
                    }

                    // Get the output "bpmn:output"
                    final NodeList bpmnOutputs = ((Element) wsdlOperation).getElementsByTagNameNS(
                            SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_OUTPUT);
                    final Templates bpmnOutputTemplate;
                    if (bpmnOutputs.getLength() == 1) {
                        final Node bpmnOutput = bpmnOutputs.item(0);
                        final String bpmnOutputStr = bpmnOutput.getTextContent();
                        if (bpmnOutputStr == null || bpmnOutputStr.isEmpty()) {
                            throw new NoOutputMappingException(wsdlOperationName);
                        } else {
                            bpmnOutputTemplate = this.readXsl(bpmnOutputStr, suRootPath, wsdlOperationName);
                        }
                    } else if (bpmnOutputs.getLength() == 0) {
                        bpmnOutputTemplate = null;
                    } else {
                        throw new DuplicatedOutputMappingException(wsdlOperationName);
                    }

                    // Create the annotated operation from annotations read into the WSDL
                    final AnnotatedOperation annotatedOperation;
                    if (StartEventAnnotatedOperation.BPMN_ACTION.equals(action)) {
                        annotatedOperation = new StartEventAnnotatedOperation(wsdlOperationName, processDefinitionKey,
                                actionId, bpmnProcessInstanceId, bpmnUserId, bpmnOperationVariables, bpmnOutputTemplate);
                    } else if (CompleteUserTaskAnnotatedOperation.BPMN_ACTION.equals(action)) {
                        annotatedOperation = new CompleteUserTaskAnnotatedOperation(wsdlOperationName,
                                processDefinitionKey, actionId, bpmnProcessInstanceId, bpmnUserId,
                                bpmnOperationVariables, bpmnOutputTemplate);
                    } else {
                        throw new UnsupportedActionException(wsdlOperationName, action);
                    }

                    // Check the coherence of the annotated operation (ie. coherence of annotations of the operation
                    // against process definitions)
                    annotatedOperation.verifyAnnotationCoherence(bpmnModels);

                    annotatedOperations.add(annotatedOperation);
                } catch (final InvalidAnnotationForOperationException e) {
                    this.encounteredErrors.add(e);
                }
            }
        }

        if (annotatedOperations.size() == 0) {
            final InvalidAnnotationException noAnnotatedOperationEx = new NoBpmnOperationException();
            this.encounteredErrors.add(noAnnotatedOperationEx);
        }

        return annotatedOperations;
    }

    /**
     * Read an XSLT style-sheet from the classloader
     * 
     * @param xslFileName
     *            The XSLT style-sheet URL
     * @param suRootPath
     *            The root directory of the service unit
     * @param wsdlOperationName
     *            The WSDL binding operation of the XSL to read
     * @return The XSLT style-sheet compiled
     * @throws InvalidOutputXslException
     *             The XSLT style-sheet read is invalid
     * @throws OutputXslNotFoundException
     *             The XSLT style-sheet was not found
     */
    private Templates readXsl(final String xslFileName, final String suRootPath, final String wsdlOperationName)
            throws InvalidOutputXslException, OutputXslNotFoundException {

        final URL xslUrl;
        // Try to get the XSL from classloader
        final URL xslUrlClassloader = Thread.currentThread().getContextClassLoader().getResource(xslFileName);
        if (xslUrlClassloader == null) {
            // Try to get the XSL from SU root path
            final File xslFile = new File(suRootPath, xslFileName);
            if (!xslFile.exists()) {
                throw new OutputXslNotFoundException(wsdlOperationName, xslFileName);
            } else {
                try {
                    xslUrl = xslFile.toURI().toURL();
                } catch (final MalformedURLException e) {
                    // This exception should never occur
                    throw new InvalidOutputXslException(wsdlOperationName, xslFileName, e);
                }
            }

        } else {
            xslUrl = xslUrlClassloader;
        }

        // Compile the XSLT style-sheet
        final TransformerFactory transformerFactory = TransformerFactory.newInstance();
        try {
            final InputStream isXsl = xslUrl.openStream();
            try {
                final Source fileSource = new StreamSource(isXsl, xslUrl.toURI().toASCIIString());
                transformerFactory.setErrorListener(this.transformerFactoryErrorListener);
                return transformerFactory.newTemplates(fileSource);
            } catch (final TransformerConfigurationException e) {
                throw new InvalidOutputXslException(wsdlOperationName, xslFileName, e);
            } catch (final URISyntaxException e) {
                throw new InvalidOutputXslException(wsdlOperationName, xslFileName, e);
            } finally {
                try {
                    isXsl.close();
                } catch (final IOException e) {
                    // NOP
                }
            }
        } catch (final IOException e) {
            throw new InvalidOutputXslException(wsdlOperationName, xslFileName, e);
        }
    }

    /**
     * Get the errors encountered during the previous parsing. Not thread-safe with the parsing itself.
     * 
     * @return The errors encountered during the previous parsing
     */
    public List<InvalidAnnotationException> getEncounteredErrors() {
        return this.encounteredErrors;
    }

}
