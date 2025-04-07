/**
 * Copyright (c) 2015-2025 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.incoming.operation.annotated;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
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

import org.flowable.bpmn.model.BpmnModel;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.DuplicatedFaultMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.DuplicatedOutputMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.DuplicatedVariableException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.FaultXslNotFoundException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidBpmnActionAttributesException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidFaultXslException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidOutputXslException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.MultipleBpmnOperationDefinedException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoBpmnOperationDefinedException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoBpmnOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoFaultMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoFaultNameMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoOutputMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoVariableMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoWsdlBindingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.OutputXslNotFoundException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.ProcessInstanceIdMappingExpressionException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.UnsupportedActionException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.UserIdMappingExpressionException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.VariableJsonNsMappingPrefixMissingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.VariableMappingExpressionException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.VariableNameMissingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.XslInvalidException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.XslNotFileException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.XslNotFoundException;
import org.ow2.petals.flowable.incoming.variable.VariableDefinition;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ebmwebsourcing.easycommons.xml.QNameHelper;

/**
 * The parser to read anotation included into the WSDL
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class AnnotatedWsdlParser {

    private static final String SCHEMA_WSDL = "http://schemas.xmlsoap.org/wsdl/";

    private static final String SCHEMA_BPMN_ANNOTATIONS = "http://petals.ow2.org/se/flowable/annotations/1.0";

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
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the identifier of the step 'none
     * start event' on which the action will be realized
     */
    private static final String BPMN_ANNOTATION_NONE_START_EVENT = "none-start-event-id";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the message name of the step
     * 'message start event' on which the action will be realized
     */
    private static final String BPMN_ANNOTATION_START_EVENT_MESSAGE_NAME = "start-event-message-name";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the message name of the step
     * 'intermediate message catch event' on which the action will be realized
     */
    private static final String BPMN_ANNOTATION_MESSAGE_EVENT_NAME = "message-event-name";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_OPERATION} containing the user task identifier on which
     * the action will be realized
     */
    private static final String BPMN_ANNOTATION_USER_TASK_ID = "user-task-id";

    /**
     * Local part of the annotation tag associated to the place holder containing the process instance identifier
     */
    private static final String BPMN_ANNOTATION_PROCESS_INSTANCE_ID_HOLDER = "processInstanceId";

    /**
     * Local part of the annotation tag associated to the place holder containing the user identifier
     */
    private static final String BPMN_ANNOTATION_USER_ID_HOLDER = "userId";

    /**
     * Local part of the annotation tag associated to a variable
     */
    private static final String BPMN_ANNOTATION_VARIABLE = "variable";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the variable name
     */
    private static final String BPMN_ANNOTATION_VARIABLE_NAME = "name";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the variable type
     */
    private static final String BPMN_ANNOTATION_VARIABLE_TYPE = "type";

    /**
     * For variable type 'json', local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the XSL
     * pre-transformaing the XML payload before conversion in JSON.
     */
    private static final String BPMN_ANNOTATION_VARIABLE_JSON_PRE_XML_TRANSFORMATION = "json-pre-xsl";

    /**
     * For variable type 'json', local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the flag
     * defining if the output of the XSL pre-transformaing the XML payload is in XML or JSON.
     */
    private static final String BPMN_ANNOTATION_VARIABLE_JSON_IS_PRE_XML_TRANSFORMATION_RESULT_JSON = "json-pre-xsl-result-json";

    /**
     * For variable type 'json', local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the virtual
     * root to use converting the variable as XML into JSON
     */
    private static final String BPMN_ANNOTATION_VARIABLE_JSON_VIRTUAL_ROOT = "json-virtual-root";

    /**
     * For variable type 'json', local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE} containing the use of
     * multiple process instructions to convert the variable as XML into JSON
     */
    private static final String BPMN_ANNOTATION_VARIABLE_JSON_MULTIPLE_PI = "json-multiple-pi";

    /**
     * For variable type 'json', local part of the annotation tag associated to the namespace mapping definition of a
     * variable ({@link #BPMN_ANNOTATION_VARIABLE}), to convert the variable as XML into JSON
     */
    private static final String BPMN_ANNOTATION_VARIABLE_JSON_NAMESPACE_MAPPING = "json-ns-mapping";

    /**
     * For variable type 'json', local part of the attribute of {@link #BPMN_ANNOTATION_VARIABLE_JSON_NAMESPACE_MAPPING}
     * defining the prefix of a namespace mapping definition used to convert the variable as XML into JSON
     */
    private static final String BPMN_ANNOTATION_VARIABLE_JSON_NS_MAPPING_PREFIX = "prefix";

    /**
     * Local part of the annotation tag associated an output
     */
    private static final String BPMN_ANNOTATION_OUTPUT = "output";

    /**
     * Local part of the annotation tag associated a fault
     */
    private static final String BPMN_ANNOTATION_FAULT = "fault";

    /**
     * Local part of the attribute of {@link #BPMN_ANNOTATION_FAULT} containing the name of the exception mapped on this
     * fault
     */
    private static final String BPMN_ANNOTATION_FAULT_NAME = "name";

    /**
     * The tenant identifier in which the process definition is deployed
     */
    protected final String tenantId;

    @SuppressWarnings("squid:S1312")
    private final Logger logger;

    /**
     * List of errors encountered during a parsing. All errors are reseted when the parsing starts
     */
    private final List<InvalidAnnotationException> encounteredErrors = new ArrayList<>();

    private final ErrorListener transformerFactoryErrorListener;

    public AnnotatedWsdlParser(final String tenantId, final Logger logger) {
        this.tenantId = tenantId;
        this.logger = logger;
        this.transformerFactoryErrorListener = new ErrorListener() {
            @Override
            public void warning(final TransformerException exception) throws TransformerException {
                AnnotatedWsdlParser.this.logger.warning(exception.getMessageAndLocation());
            }

            @Override
            public void fatalError(final TransformerException exception) throws TransformerException {
                this.error(exception);
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
     *            The WSDL to parse containing BPMN annotation. Not {@code null}.
     * @param bpmnModels
     *            BPMN models embedded into the service unit. Not {@code null}.
     * @param suRootPath
     *            The root directory of the service unit
     * @param logErrorListener
     *            Error listener of the XSL engine. Not {@code null}.
     * @return For each operation of the WSDL, the associated annotated operation is returned.
     */
    public List<AnnotatedOperation> parse(final Document annotatedWsdl, final List<BpmnModel> bpmnModels,
            final String suRootPath, final ErrorListener logErrorListener) {

        assert annotatedWsdl != null;
        assert bpmnModels != null;

        this.encounteredErrors.clear();

        final List<AnnotatedOperation> annotatedOperations = new ArrayList<>();

        annotatedWsdl.getDocumentElement().normalize();
        final String targetNamespace = annotatedWsdl.getDocumentElement().getAttribute("targetNamespace");
        final XPathFactory xpathFactory = XPathFactory.newInstance();
        final XPath xpathBuilder = xpathFactory.newXPath();

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
                    annotatedOperations.add(this.parseOperation(wsdlOperations.item(j), targetNamespace, xpathBuilder,
                            suRootPath, bpmnModels, logErrorListener));
                } catch (final InvalidAnnotationForOperationException e) {
                    this.encounteredErrors.add(e);
                }
            }
        }

        if (annotatedOperations.isEmpty()) {
            final InvalidAnnotationException noAnnotatedOperationEx = new NoBpmnOperationException();
            this.encounteredErrors.add(noAnnotatedOperationEx);
        }

        return annotatedOperations;
    }

    /**
     * Parse a WSDL binding operation
     * 
     * @param wsdlOperation
     *            The binding operation to parse
     * @param targetNamespace
     *            The target namespace of WSDL definition
     * @param xpathBuilder
     *            The XPath expression builder
     * @param suRootPath
     *            The root directory of the service unit
     * @param bpmnModels
     *            BPMN models embedded into the service unit
     * @param logErrorListener
     *            Error listener of the XSL engine. Not {@code null}.
     * @return The annotated operation associated to the WSDL binding operation
     * @throws An
     *             error occurs during the parsing of the binding operation
     */
    private AnnotatedOperation parseOperation(final Node wsdlOperation, final String targetNamespace,
            final XPath xpathBuilder, final String suRootPath, final List<BpmnModel> bpmnModels,
            final ErrorListener logErrorListener) throws InvalidAnnotationForOperationException {

        final QName wsdlOperationName = new QName(targetNamespace, ((Element) wsdlOperation).getAttribute("name"));

        // Get the node "bpmn:operation"
        final NodeList bpmnOperations = ((Element) wsdlOperation).getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS,
                BPMN_ANNOTATION_OPERATION);
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

        // Get the node "bpmn:userId"
        final XPathExpression bpmnUserId = this.getUserIdXpathExpr(wsdlOperation, wsdlOperationName, xpathBuilder);

        // Get the list of nodes "bpmn:variable"
        final Map<String, VariableDefinition> bpmnOperationVariables = this.getVariables(wsdlOperation,
                wsdlOperationName, xpathBuilder, suRootPath, logErrorListener);

        // Get the output "bpmn:output"
        final Templates bpmnOutputTemplate = this.getOutputTemplate(wsdlOperation, wsdlOperationName, suRootPath);

        // Get the faults
        final Map<String, Templates> bpmnFaultTemplates = this.getFaultTemplates(wsdlOperation, wsdlOperationName,
                suRootPath);

        // Create the annotated operation from annotations read into the WSDL
        final AnnotatedOperation annotatedOperation;
        if (StartEventAnnotatedOperation.BPMN_ACTION.equals(action)) {

            // if none start event, get the start event id on which the action must be done
            final String noneStartEventId = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_NONE_START_EVENT);

            // if message start event, get the message name on which the action must be done
            final String startEventMessageName = ((Element) bpmnOperation)
                    .getAttribute(BPMN_ANNOTATION_START_EVENT_MESSAGE_NAME);

            if (!noneStartEventId.isEmpty() && startEventMessageName.isEmpty()) {
                annotatedOperation = new NoneStartEventAnnotatedOperation(wsdlOperationName, processDefinitionKey,
                        noneStartEventId, bpmnUserId, bpmnOperationVariables, bpmnOutputTemplate, bpmnFaultTemplates);
            } else if (noneStartEventId.isEmpty() && !startEventMessageName.isEmpty()) {
                annotatedOperation = new MessageStartEventAnnotatedOperation(wsdlOperationName, processDefinitionKey,
                        startEventMessageName, this.tenantId, bpmnUserId, bpmnOperationVariables, bpmnOutputTemplate,
                        bpmnFaultTemplates);
            } else {
                throw new InvalidBpmnActionAttributesException(wsdlOperationName,
                        StartEventAnnotatedOperation.BPMN_ACTION);
            }
        } else if (CompleteUserTaskAnnotatedOperation.BPMN_ACTION.equals(action)) {

            // get the user task id on which the action must be done
            final String userTaskId = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_USER_TASK_ID);

            // Get the node "bpmn:processId" and its message
            final XPathExpression bpmnProcessInstanceId = this.getProcessInstanceIdXpathExpr(wsdlOperation,
                    wsdlOperationName, xpathBuilder);

            annotatedOperation = new CompleteUserTaskAnnotatedOperation(wsdlOperationName, processDefinitionKey,
                    userTaskId, bpmnProcessInstanceId, bpmnUserId, bpmnOperationVariables, bpmnOutputTemplate,
                    bpmnFaultTemplates);
        } else if (IntermediateMessageCatchEventAnnotatedOperation.BPMN_ACTION.equals(action)) {

            // get the intermediate message catch event id on which the action must be done
            final String messageEventId = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_MESSAGE_EVENT_NAME);

            // Get the node "bpmn:processId" and its message
            final XPathExpression bpmnProcessInstanceId = this.getProcessInstanceIdXpathExpr(wsdlOperation,
                    wsdlOperationName, xpathBuilder);

            annotatedOperation = new IntermediateMessageCatchEventAnnotatedOperation(wsdlOperationName,
                    processDefinitionKey, bpmnProcessInstanceId, messageEventId, bpmnOperationVariables,
                    bpmnFaultTemplates);

        } else {
            throw new UnsupportedActionException(wsdlOperationName, action);
        }

        // Check the coherence of the annotated operation (ie. coherence of annotations of the operation
        // against process definitions)
        annotatedOperation.verifyAnnotationCoherence(bpmnModels);

        return annotatedOperation;
    }

    /**
     * Parse the process instance identifier defined for the binding operation
     * 
     * @param wsdlOperation
     *            The node of the binding operation to parse
     * @param wsdlOperationName
     *            The name of the binding operation
     * @param xpathBuilder
     *            The XPath expression builder
     * @return The XPath expression to select the process instance identifier
     * @throws InvalidAnnotationForOperationException
     */
    private XPathExpression getProcessInstanceIdXpathExpr(final Node wsdlOperation, final QName wsdlOperationName,
            final XPath xpathBuilder) throws InvalidAnnotationForOperationException {

        final Node processInstanceId = ((Element) wsdlOperation)
                .getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_PROCESS_INSTANCE_ID_HOLDER).item(0);
        final XPathExpression bpmnProcessInstanceId;
        if (processInstanceId != null) {
            final String xpathExpr = processInstanceId.getTextContent();
            if (xpathExpr.trim().isEmpty()) {
                throw new NoProcessInstanceIdMappingException(wsdlOperationName);
            } else {
                try {
                    bpmnProcessInstanceId = xpathBuilder.compile(xpathExpr);
                } catch (final XPathExpressionException e) {
                    throw new ProcessInstanceIdMappingExpressionException(wsdlOperationName, e);
                }
            }
        } else {
            bpmnProcessInstanceId = null;
        }

        return bpmnProcessInstanceId;
    }

    /**
     * Parse the user identifier defined for the binding operation
     * 
     * @param wsdlOperation
     *            The node of the binding operation to parse
     * @param wsdlOperationName
     *            The name of the binding operation
     * @param xpathBuilder
     *            The XPath expression builder
     * @return The XPath expression to select the user identifier
     * @throws InvalidAnnotationForOperationException
     */
    private XPathExpression getUserIdXpathExpr(final Node wsdlOperation, final QName wsdlOperationName,
            final XPath xpathBuilder) throws InvalidAnnotationForOperationException {

        final Node userId = ((Element) wsdlOperation)
                .getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_USER_ID_HOLDER).item(0);
        final XPathExpression bpmnUserId;
        if (userId != null) {
            final String xpathExpr = userId.getTextContent();
            if (xpathExpr.trim().isEmpty()) {
                throw new NoUserIdMappingException(wsdlOperationName);
            } else {
                try {
                    bpmnUserId = xpathBuilder.compile(xpathExpr);
                } catch (final XPathExpressionException e) {
                    throw new UserIdMappingExpressionException(wsdlOperationName, e);
                }
            }
        } else {
            bpmnUserId = null;
        }

        return bpmnUserId;
    }

    /**
     * Parse the variable defined for the binding operation
     * 
     * @param wsdlOperation
     *            The node of the binding operation to parse
     * @param wsdlOperationName
     *            The name of the binding operation
     * @param xpathBuilder
     *            The XPath expression builder
     * @param suRootPath
     *            Root directory of the service unit deployed
     * @param logErrorListener
     *            Error listener of the XSL engine. Not {@code null}.
     * @return The XPath expression for each variable defined
     * @throws InvalidAnnotationForOperationException
     */
    private Map<String, VariableDefinition> getVariables(final Node wsdlOperation, final QName wsdlOperationName,
            final XPath xpathBuilder, final String suRootPath, final ErrorListener logErrorListener)
            throws InvalidAnnotationForOperationException {

        final NodeList bpmnVariableList = ((Element) wsdlOperation).getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS,
                BPMN_ANNOTATION_VARIABLE);
        final Map<String, VariableDefinition> bpmnOperationVariables = new HashMap<>();
        for (int k = 0; k < bpmnVariableList.getLength(); k++) {
            final Node bpmnVariable = bpmnVariableList.item(k);

            final String bpmnVariableName = ((Element) bpmnVariable).getAttribute(BPMN_ANNOTATION_VARIABLE_NAME);
            if (bpmnVariableName == null || bpmnVariableName.trim().isEmpty()) {
                throw new VariableNameMissingException(wsdlOperationName);
            }
            // Check unicity of the declared variable for this operation
            if (bpmnOperationVariables.containsKey(bpmnVariableName)) {
                throw new DuplicatedVariableException(wsdlOperationName, bpmnVariableName);
            }

            final XPathExpression bpmnVariableXPath;
            final Node xpathExprNode = bpmnVariable.getFirstChild();
            if (xpathExprNode == null) {
                throw new NoVariableMappingException(wsdlOperationName, bpmnVariableName);
            } else {
                final String xpathExpr = bpmnVariable.getFirstChild().getTextContent();
                if (xpathExpr.trim().isEmpty()) {
                    throw new NoVariableMappingException(wsdlOperationName, bpmnVariableName);
                } else {
                    try {
                        bpmnVariableXPath = xpathBuilder.compile(xpathExpr.trim());
                    } catch (final XPathExpressionException e) {
                        throw new VariableMappingExpressionException(wsdlOperationName, bpmnVariableName, e);
                    }
                }
            }

            final String bpmnVariableType = ((Element) bpmnVariable).getAttribute(BPMN_ANNOTATION_VARIABLE_TYPE);

            final VariableDefinition variableDefinition;
            if (bpmnVariableType != null && !bpmnVariableType.trim().isEmpty()) {
                variableDefinition = new VariableDefinition(bpmnVariableName, bpmnVariableXPath,
                        bpmnVariableType.trim());
            } else {
                variableDefinition = new VariableDefinition(bpmnVariableName, bpmnVariableXPath);
            }

            final String preXmlTransformationStr = ((Element) bpmnVariable)
                    .getAttribute(BPMN_ANNOTATION_VARIABLE_JSON_PRE_XML_TRANSFORMATION);
            if (preXmlTransformationStr != null && !preXmlTransformationStr.trim().isEmpty()) {
                variableDefinition.setPreXmlTransformation(this.buildXslTemplate(wsdlOperationName, bpmnVariableName,
                        suRootPath, preXmlTransformationStr.trim(), logErrorListener));
            }

            final String isPreXmlTranformationResultJsonStr = ((Element) bpmnVariable)
                    .getAttribute(BPMN_ANNOTATION_VARIABLE_JSON_IS_PRE_XML_TRANSFORMATION_RESULT_JSON);
            if (isPreXmlTranformationResultJsonStr != null && !isPreXmlTranformationResultJsonStr.trim().isEmpty()) {
                variableDefinition.setPreXmlTranformationResultJson(
                        Boolean.parseBoolean(isPreXmlTranformationResultJsonStr.toLowerCase()));
            } else {
                variableDefinition.setPreXmlTranformationResultJson(false);
            }

            final String virtRootStr = ((Element) bpmnVariable)
                    .getAttribute(BPMN_ANNOTATION_VARIABLE_JSON_VIRTUAL_ROOT);
            if (virtRootStr != null && !virtRootStr.trim().isEmpty()) {
                variableDefinition
                        .setVirtualRoot(
                                QNameHelper.fromString(bpmnVariable, virtRootStr.trim(), XMLConstants.NULL_NS_URI));
            }

            final String multiplePiStr = ((Element) bpmnVariable)
                    .getAttribute(BPMN_ANNOTATION_VARIABLE_JSON_MULTIPLE_PI);
            if (multiplePiStr != null && !multiplePiStr.trim().isEmpty()) {
                variableDefinition.setMultiplePi(Optional.of(Boolean.parseBoolean(multiplePiStr)));
            }

            final NodeList namespaceMappingNodes = ((Element) bpmnVariable)
                    .getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS, BPMN_ANNOTATION_VARIABLE_JSON_NAMESPACE_MAPPING);
            final Map<String, String> namespaceMappings = new HashMap<>(namespaceMappingNodes.getLength());
            for (int i = 0; i < namespaceMappingNodes.getLength(); i++) {
                final Node namespaceMappingNode = namespaceMappingNodes.item(i);
                if (namespaceMappingNode instanceof Element) {
                    final String prefix = ((Element) namespaceMappingNode)
                            .getAttribute(BPMN_ANNOTATION_VARIABLE_JSON_NS_MAPPING_PREFIX);
                    if (prefix == null || prefix.trim().isEmpty()) {
                        throw new VariableJsonNsMappingPrefixMissingException(wsdlOperationName, bpmnVariableName);
                    }

                    final String uri = namespaceMappingNode.getTextContent();

                    namespaceMappings.put(prefix, uri);
                }
            }
            variableDefinition.setNamespaceMappings(namespaceMappings);

            bpmnOperationVariables.put(bpmnVariableName, variableDefinition);
        }

        return bpmnOperationVariables;
    }

    private Templates buildXslTemplate(final QName wsdlOperationName, final String bpmnVariableName,
            final String suRootPath, final String xslFileName, final ErrorListener logErrorListener)
            throws InvalidAnnotationForOperationException {

        // The XSL style-sheet file must exist
        final File xslFile = new File(suRootPath, xslFileName);
        if (!xslFile.exists()) {
            throw new XslNotFoundException(wsdlOperationName, bpmnVariableName, xslFile.getAbsolutePath());
        }

        // The XSL style-sheet file must be a regular file
        if (!xslFile.isFile()) {
            throw new XslNotFileException(wsdlOperationName, bpmnVariableName, xslFile.getAbsolutePath());
        }

        // The XSL style-sheet file must be a buildable XSL
        final Source xslSource = new StreamSource(xslFile);
        final TransformerFactory transformerFactory = TransformerFactory.newInstance();
        transformerFactory.setErrorListener(logErrorListener);
        try {
            return transformerFactory.newTemplates(xslSource);
        } catch (final TransformerConfigurationException e) {
            throw new XslInvalidException(wsdlOperationName, bpmnVariableName, xslFile.getAbsolutePath(), e);
        }
    }

    /**
     * Parse the annotation defining the output of the operation
     * 
     * @param wsdlOperation
     *            The node of the binding operation to parse
     * @param wsdlOperationName
     *            The name of the binding operation
     * @param suRootPath
     *            The root directory of the service unit
     * @return The output XSLT style-sheet compiled
     * @throws InvalidAnnotationForOperationException
     */
    private Templates getOutputTemplate(final Node wsdlOperation, final QName wsdlOperationName,
            final String suRootPath) throws InvalidAnnotationForOperationException {

        final NodeList bpmnOutputs = ((Element) wsdlOperation).getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS,
                BPMN_ANNOTATION_OUTPUT);
        final Templates bpmnOutputTemplate;
        if (bpmnOutputs.getLength() == 1) {
            final Node bpmnOutput = bpmnOutputs.item(0);
            final String bpmnOutputStr = bpmnOutput.getTextContent();
            if (bpmnOutputStr == null || bpmnOutputStr.isEmpty()) {
                throw new NoOutputMappingException(wsdlOperationName);
            } else {
                bpmnOutputTemplate = this.readOutputXsl(bpmnOutputStr.trim(), suRootPath, wsdlOperationName);
            }
        } else if (bpmnOutputs.getLength() == 0) {
            bpmnOutputTemplate = null;
        } else {
            throw new DuplicatedOutputMappingException(wsdlOperationName);
        }

        return bpmnOutputTemplate;
    }

    /**
     * Parse the annotation defining the faults of the operation
     * 
     * @param wsdlOperation
     *            The node of the binding operation to parse
     * @param wsdlOperationName
     *            The name of the binding operation
     * @param suRootPath
     *            The root directory of the service unit
     * @return The fault XSLT style-sheets compiled
     * @throws InvalidAnnotationForOperationException
     */
    private Map<String, Templates> getFaultTemplates(final Node wsdlOperation, final QName wsdlOperationName,
            final String suRootPath) throws InvalidAnnotationForOperationException {
        
        final Map<String, Templates> faultTemplates = new HashMap<>();
        
        final NodeList wsdlFaults = ((Element) wsdlOperation).getElementsByTagNameNS(SCHEMA_WSDL, "fault");
        for (int j = 0; j < wsdlFaults.getLength(); j++) {
            final Node wsdlFault = wsdlFaults.item(j);
            final String wsdlFaultName = ((Element) wsdlFault).getAttribute("name");

            final NodeList bpmnFaults = ((Element) wsdlFault).getElementsByTagNameNS(SCHEMA_BPMN_ANNOTATIONS,
                    BPMN_ANNOTATION_FAULT);
            if (bpmnFaults.getLength() == 1) {
                final Element bpmnFault = (Element) bpmnFaults.item(0);
                final String bpmnFaultName = bpmnFault.getAttribute(BPMN_ANNOTATION_FAULT_NAME);
                if (bpmnFaultName == null || bpmnFaultName.trim().isEmpty()) {
                    throw new NoFaultNameMappingException(wsdlOperationName, wsdlFaultName);
                }
                final String bpmnFaultStr = bpmnFault.getTextContent();
                if (bpmnFaultStr == null || bpmnFaultStr.isEmpty()) {
                    throw new NoFaultMappingException(wsdlOperationName, wsdlFaultName);
                } else {
                    faultTemplates.put(bpmnFaultName,
                            this.readFaultXsl(bpmnFaultStr, suRootPath, wsdlOperationName, wsdlFaultName));
                }
            } else if (bpmnFaults.getLength() == 0) {
                throw new NoFaultMappingException(wsdlOperationName, wsdlFaultName);
            } else if (bpmnFaults.getLength() > 1) {
                throw new DuplicatedFaultMappingException(wsdlOperationName, wsdlFaultName);
            }
        }

        return faultTemplates;
    }

    /**
     * Read an output XSLT style-sheet from the classloader
     * 
     * @param xslFileName
     *            The XSLT style-sheet filename. Not {@code null}, not empty, and trimmed.
     * @param suRootPath
     *            The root directory of the service unit
     * @param wsdlOperationName
     *            The WSDL binding operation of the XSL to read
     * @return The XSLT style-sheet compiled
     * @throws InvalidOutputXslException
     *             The output XSLT style-sheet read is invalid
     * @throws OutputXslNotFoundException
     *             The output XSLT style-sheet was not found
     */
    private Templates readOutputXsl(final String xslFileName, final String suRootPath, final QName wsdlOperationName)
            throws InvalidOutputXslException, OutputXslNotFoundException {

        assert xslFileName != null;
        assert !xslFileName.isEmpty();

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
                    // The Java URI class allows non-URI characters that are not allowed by RFC 3896. The
                    // URI returned by File.toURI() is invalid except using URI.toASCIIString().
                    // see: http://www.garretwilson.com/blog/2008/10/25/javauriclassgotchas.xhtml
                    xslUrl = URI.create(xslFile.toURI().toASCIIString()).toURL();
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
            } catch (final TransformerConfigurationException | URISyntaxException e) {
                throw new InvalidOutputXslException(wsdlOperationName, xslFileName, e);
            } finally {
                try {
                    isXsl.close();
                } catch (final IOException e) {
                    this.logger.log(Level.WARNING,
                            String.format("An error occurs closing XSL '%s'. Error skipped.", xslUrl.toString()), e);
                }
            }
        } catch (final IOException e) {
            throw new InvalidOutputXslException(wsdlOperationName, xslFileName, e);
        }
    }

    /**
     * Read a fault XSLT style-sheet from the classloader
     * 
     * @param xslFileName
     *            The XSLT style-sheet URL
     * @param suRootPath
     *            The root directory of the service unit
     * @param wsdlOperationName
     *            The WSDL binding operation of the XSL to read
     * @param wsdlFaultName
     *            The WSDL fault name into the WSDL binding operation for which the XSL is read
     * @return The XSLT style-sheet compiled
     * @throws InvalidFaultXslException
     *             The fault XSLT style-sheet read is invalid
     * @throws FaultXslNotFoundException
     *             The fault XSLT style-sheet was not found
     */
    private Templates readFaultXsl(final String xslFileName, final String suRootPath, final QName wsdlOperationName,
            final String wsdlFaultName) throws InvalidFaultXslException, FaultXslNotFoundException {

        final URL xslUrl;
        // Try to get the XSL from classloader
        final URL xslUrlClassloader = Thread.currentThread().getContextClassLoader().getResource(xslFileName);
        if (xslUrlClassloader == null) {
            // Try to get the XSL from SU root path
            final File xslFile = new File(suRootPath, xslFileName);
            if (!xslFile.exists()) {
                throw new FaultXslNotFoundException(wsdlOperationName, wsdlFaultName, xslFileName);
            } else {
                try {
                    // The Java URI class allows non-URI characters that are not allowed by RFC 3896. The
                    // URI returned by File.toURI() is invalid except using URI.toASCIIString().
                    // see: http://www.garretwilson.com/blog/2008/10/25/javauriclassgotchas.xhtml
                    xslUrl = URI.create(xslFile.toURI().toASCIIString()).toURL();
                } catch (final MalformedURLException e) {
                    // This exception should never occur
                    throw new InvalidFaultXslException(wsdlOperationName, wsdlFaultName, xslFileName, e);
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
            } catch (final TransformerConfigurationException | URISyntaxException e) {
                throw new InvalidFaultXslException(wsdlOperationName, wsdlFaultName, xslFileName, e);
            } finally {
                try {
                    isXsl.close();
                } catch (final IOException e) {
                    this.logger.log(Level.WARNING,
                            String.format("An error occurs closing XSL '%s'. Error skipped.", xslUrl.toString()), e);
                }
            }
        } catch (final IOException e) {
            throw new InvalidFaultXslException(wsdlOperationName, wsdlFaultName, xslFileName, e);
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
