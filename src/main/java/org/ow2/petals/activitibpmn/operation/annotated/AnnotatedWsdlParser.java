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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.ow2.petals.activitibpmn.operation.annotated.exception.DuplicatedVariableException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.MultipleBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationDefinedException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoBpmnOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoWsdlBindingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.ProcessInstanceIdMappingExpressionException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UnsupportedBpmnActionTypeException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UserIdMappingExpressionException;
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

    private static final String BPMN_ANNOTATION_ACTION = "bpmnAction";

    /**
     * Local part of the annotation tag associated to the action type to do on BPMN engine
     */
    private static final String BPMN_ANNOTATION_ACTION_TYPE = "bpmnActionType";

    /**
     * Local part of the annotation tag associated to the place holder containing the process instance identifier
     */
    private static final String BPMN_ANNOTATION_PROCESS_INSTANCE_ID_HOLDER = "processInstanceId";

    /**
     * Local part of the annotation tag associated to the place holder containing the user identifier
     */
    private static final String BPMN_ANNOTATION_USER_ID_HOLDER = "userId";

    private final Logger logger;

    /**
     * List of errors encountered during a parsing. All errors are reseted when the parsing starts
     */
    private final List<InvalidAnnotationException> encounteredErrors = new ArrayList<InvalidAnnotationException>();

    public AnnotatedWsdlParser(final Logger logger) {
        this.logger = logger;
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
     * @return For each operation of the WSDL, the associated annotated operation is returned.
     */
    public List<AnnotatedOperation> parse(final Document annotatedWsdl) {

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

                    // get the bpmnAction
                    final String bpmnAction = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_ACTION);

                    // get the bpmnActionType
                    final String bpmnActionType = ((Element) bpmnOperation).getAttribute(BPMN_ANNOTATION_ACTION_TYPE);

                    // Get the node "bpmn:processId" and its message
                    final Node processInstanceId = ((Element) wsdlOperation).getElementsByTagNameNS(
                            SCHEMA_BPMN_ANNOTATIONS,
                            BPMN_ANNOTATION_PROCESS_INSTANCE_ID_HOLDER).item(0);
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
                            SCHEMA_BPMN_ANNOTATIONS, "variable");
                    final Properties bpmnVarInMsg = new Properties();
                    final Properties outMsgBpmnVar = new Properties();
                    final Properties faultMsgBpmnVar = new Properties();
                    final Set<String> bpmnVarList = new HashSet<String>();
                    for (int k = 0; k < bpmnVariableList.getLength(); k++) {
                        final Node bpmnVariable = bpmnVariableList.item(k);
                        // test name declaration of variable
                        final String bpmnAttr = ((Element) bpmnVariable).getAttribute("bpmn");
                        if (bpmnAttr == null) {
                            throw new VariableNameMissingException(wsdlOperationName);
                        }
                        // test unicity of declared bpmnVariable
                        if (bpmnVarList.contains(bpmnAttr)) {
                            throw new DuplicatedVariableException(wsdlOperationName, bpmnAttr);
                        }
                        // Add bpmnVariables in the bpmnVarList
                        bpmnVarList.add(bpmnAttr);
                        // Add bpmnVariables
                        final String inMsgAttr = ((Element) bpmnVariable).getAttribute("inMsg");
                        if (inMsgAttr != null && !inMsgAttr.isEmpty()) {
                            bpmnVarInMsg.put(bpmnAttr, inMsgAttr);
                        }

                        final String outMsgAttr = ((Element) bpmnVariable).getAttribute("outMsg");
                        if (outMsgAttr != null && !outMsgAttr.isEmpty()) {
                            outMsgBpmnVar.put(outMsgAttr, bpmnAttr);
                        }

                        final String faultMsgAttr = ((Element) bpmnVariable).getAttribute("faultMsg");
                        if (faultMsgAttr != null && !faultMsgAttr.isEmpty()) {
                            faultMsgBpmnVar.put(faultMsgAttr, bpmnAttr);
                        }
                    }

                    // Create the annotated operation from annotations read into the WSDL
                    final AnnotatedOperation annotatedOperation;
                    if (StartEventAnnotatedOperation.BPMN_ACTION_TYPE.equals(bpmnActionType)) {
                        annotatedOperation = new StartEventAnnotatedOperation(wsdlOperationName, processDefinitionKey,
                                bpmnAction, bpmnProcessInstanceId, bpmnUserId, bpmnVarInMsg, outMsgBpmnVar,
                                faultMsgBpmnVar, bpmnVarList);
                    } else if (CompleteUserTaskAnnotatedOperation.BPMN_ACTION_TYPE.equals(bpmnActionType)) {
                        annotatedOperation = new CompleteUserTaskAnnotatedOperation(wsdlOperationName,
                                processDefinitionKey, bpmnAction, bpmnProcessInstanceId, bpmnUserId, bpmnVarInMsg,
                                outMsgBpmnVar, faultMsgBpmnVar, bpmnVarList);
                    } else {
                        throw new UnsupportedBpmnActionTypeException(wsdlOperationName, bpmnAction);
                    }

                    // Check the coherence of the annotated operation (ie. coherence of annotations of the operation)
                    annotatedOperation.verifyAnnotationCoherence();

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
     * Get the errors encountered during the previous parsing. Not thread-safe with the parsing itself.
     * 
     * @return The errors encountered during the previous parsing
     */
    public List<InvalidAnnotationException> getEncounteredErrors() {
        return this.encounteredErrors;
    }

}
