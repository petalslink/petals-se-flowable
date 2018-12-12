/**
 * Copyright (c) 2015-2018 Linagora
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.flowable.bpmn.model.BpmnModel;
import org.flowable.bpmn.model.FormProperty;
import org.flowable.bpmn.model.Process;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoProcessDefinitionIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.ProcessDefinitionIdDuplicatedInModelException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.ProcessDefinitionIdNotFoundInModelException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.RequiredVariableMissingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.UnsupportedMappedExceptionNameException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.VariableNotFoundInModelException;

/**
 * A BPMN operation extracted from WDSL according to BPMN annotations
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class AnnotatedOperation {

    /**
     * The WSDL operation containing the current annotations
     */
    protected final QName wsdlOperation;

    /**
     * The BPMN process identifier associated to the BPMN operation.
     */
    private final String processDefinitionId;

    /**
     * The place holder of the incoming request containing the user identifier with which the BPMN operation must be
     * executed
     */
    protected final XPathExpression userIdHolder;

    /**
     * The definition of variables of the operation read from the WSDL
     */
    private final Map<String, XPathExpression> variables;

    /**
     * Drive the {@link AnnotatedOperation} processing against capacity of associated BPMN element to declare process
     * variables. If {@code true}, the BPMN element variables can be declared at BPMN level and a check will be done.
     */
    private final boolean canDeclareVariable;

    /**
     * Types of variables read from the process definition.
     */
    private final Map<String, FormProperty> variableTypes = new HashMap<>();

    /**
     * The XSLT style-sheet compiled associated to WSDL faults. The key is the class simple name of the exception
     * associated to the fault.
     */
    private final Map<String, Templates> faultTemplates;

    /**
     * <p>
     * Create an annotated operation.
     * </p>
     * <p>
     * <b>Note 1</b>: If the user identifier placeholder is null or empty, the error {@link NoUserIdMappingException}
     * will be thrown.
     * </p>
     * <p>
     * <b>Note 2</b>: For performance reasons, variable types are populated during the coherence check.
     * </p>
     * 
     * @param wsdlOperation
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param userIdHolder
     *            The placeholder of BPMN user identifier associated to the BPMN operation. Not <code>null</code>.
     * @param variables
     *            The definition of variables of the operation
     * @param faultTemplates
     *            The XSLT style-sheet compiled associated to WSDL faults
     * @param canDeclareVariable
     *            Drive the processing against capacity of associated BPMN element to declare process variables
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    protected AnnotatedOperation(final QName wsdlOperation, final String processDefinitionId,
            final XPathExpression userIdHolder, final Map<String, XPathExpression> variables,
            final Map<String, Templates> faultTemplates, final boolean canDeclareVariable)
            throws InvalidAnnotationForOperationException {
        super();
        this.wsdlOperation = wsdlOperation;
        this.processDefinitionId = processDefinitionId;
        this.userIdHolder = userIdHolder;
        this.variables = variables;
        this.faultTemplates = faultTemplates;
        this.canDeclareVariable = canDeclareVariable;
    }

    /**
     * Verify that annotation read from the WSDL are valid for the operation, otherwise the exception
     * {@link InvalidAnnotationForOperationException} is thrown.
     * 
     * @param bpmnModels
     *            BPMN models embedded into the service unit
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    public void verifyAnnotationCoherence(final List<BpmnModel> bpmnModels)
            throws InvalidAnnotationForOperationException {

        // The process definition identifier is required
        if (this.processDefinitionId == null || this.processDefinitionId.trim().isEmpty()) {
            throw new NoProcessDefinitionIdMappingException(this.wsdlOperation);
        }

        // Check that the process definition identifier exists only once in the process definitions embedded into the
        // service unit
        int processDefinitionIdCount = 0;
        BpmnModel modelContainingProcessDefinitionId = null;
        for (final BpmnModel bpmnModel : bpmnModels) {
            for (final Process process : bpmnModel.getProcesses()) {
                if (this.processDefinitionId.equals(process.getId())) {
                    processDefinitionIdCount++;
                    modelContainingProcessDefinitionId = bpmnModel;
                }
            }
        }
        if (processDefinitionIdCount == 0) {
            throw new ProcessDefinitionIdNotFoundInModelException(this.wsdlOperation, this.processDefinitionId);
        } else if (processDefinitionIdCount > 1) {
            throw new ProcessDefinitionIdDuplicatedInModelException(this.wsdlOperation, this.processDefinitionId);
        }

        this.doAnnotationCoherenceCheck(modelContainingProcessDefinitionId);

        // Check the existence of declared variables into the process definition
        if (this.canDeclareVariable) {
            for (final String variableName : this.variables.keySet()) {
                if (!this.variableTypes.containsKey(variableName)) {
                    throw new VariableNotFoundInModelException(this.wsdlOperation, variableName,
                            this.processDefinitionId);
                }
            }
        }

        // Check that the all required variables of the process definition are mapped
        for (final Entry<String, FormProperty> entry : this.variableTypes.entrySet()) {
            final String variableName = entry.getKey();
            if (entry.getValue().isRequired() && !this.variables.containsKey(variableName)) {
                throw new RequiredVariableMissingException(this.wsdlOperation, variableName);
            }
        }

        // Check supported fault mapping
        final List<String> mappedExceptionNames = this.getMappedExceptionNames();
        for (final String faultName : this.getFaultTemplates().keySet()) {
            if (!mappedExceptionNames.contains(faultName)) {
                throw new UnsupportedMappedExceptionNameException(this.wsdlOperation, faultName);
            }
        }
    }

    private List<String> getMappedExceptionNames() {
        final List<String> mappedExceptionNames = new ArrayList<>();
        this.addMappedExceptionNames(mappedExceptionNames);
        return mappedExceptionNames;
    }

    protected abstract void addMappedExceptionNames(final List<String> mappedExceptionNames);

    /**
     * <p>
     * Entry point to:
     * </p>
     * <ul>
     * <li>extend checks about annotation coherence,</li>
     * <li>and to populate variable types.</li>
     * </ul>
     * 
     * @param model
     *             The model of the process definition
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    public abstract void doAnnotationCoherenceCheck(final BpmnModel model)
            throws InvalidAnnotationForOperationException;

    /**
     * @return The WSDL operation containing the current annotations
     */
    public QName getWsdlOperation() {
        return this.wsdlOperation;
    }

    /**
     * @return The BPMN process definition identifier associated to the BPMN operation
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

    /**
     * @return The action to realize on the BPMN process side
     */
    public abstract String getAction();

    /**
     * @return The placeholder of BPMN user identifier associated to the BPMN operation.
     */
    public XPathExpression getUserIdHolder() {
        return this.userIdHolder;
    }

    /**
     * @return The definition of variables of the operation
     */
    public Map<String, XPathExpression> getVariables() {
        return this.variables;
    }

    /**
     * @return Types of variables
     */
    public Map<String, FormProperty> getVariableTypes() {
        return this.variableTypes;
    }

    /**
     * @return The XSLT style-sheet compiled associated to WSDL faults
     */
    public Map<String, Templates> getFaultTemplates() {
        return this.faultTemplates;
    }
}
