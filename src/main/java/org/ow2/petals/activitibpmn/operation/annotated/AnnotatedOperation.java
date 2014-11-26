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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FormProperty;
import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoActionIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoOutputMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoProcessDefinitionIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.ProcessDefinitionIdDuplicatedInModelException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.ProcessDefinitionIdNotFoundInModelException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.RequiredVariableMissingException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.UnsupportedMappedExceptionNameException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.VariableNotFoundInModelException;

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
    private final String wsdlOperationName;

    /**
     * The BPMN process identifier associated to the BPMN operation.
     */
    private final String processDefinitionId;

    /**
     * The task identifier on which the action must be realize on the BPMN process side
     */
    private final String actionId;

    /**
     * The place holder of the incoming request containing the process instance identifier on which the BPMN operation
     * must be executed
     */
    private final XPathExpression processInstanceIdHolder;

    /**
     * The place holder of the incoming request containing the user identifier with which the BPMN operation must be
     * executed
     */
    private final XPathExpression userIdHolder;

    /**
     * The definition of variables of the operation read from the WSDL
     */
    private final Map<String, XPathExpression> variables;

    /**
     * Types of variables read from the process definition.
     */
    private final Map<String, FormProperty> variableTypes = new HashMap<String, FormProperty>();

    /**
     * The output XSLT style-sheet compiled
     */
    private final Templates outputTemplate;

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
     * <b>Note 2</b>: For performance reasons, variable types are populated during the coherence check. </p>
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param actionId
     *            The task identifier on which the action must be realize on the BPMN process side
     * @param processInstanceIdHolder
     *            The placeholder of BPMN process instance identifier associated to the BPMN operation. Not
     *            <code>null</code>.
     * @param userIdHolder
     *            The placeholder of BPMN user identifier associated to the BPMN operation. Not <code>null</code>.
     * @param variables
     *            The definition of variables of the operation
     * @param outputTemplate
     *            The output XSLT style-sheet compiled
     * @param faultTemplates
     *            The XSLT style-sheet compiled associated to WSDL faults
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    protected AnnotatedOperation(final String wsdlOperationName, final String processDefinitionId,
            final String actionId, final XPathExpression processInstanceIdHolder, final XPathExpression userIdHolder,
            final Map<String, XPathExpression> variables, final Templates outputTemplate,
            final Map<String, Templates> faultTemplates) throws InvalidAnnotationForOperationException {
        super();
        this.wsdlOperationName = wsdlOperationName;
        this.processDefinitionId = processDefinitionId;
        this.actionId = actionId;
        this.processInstanceIdHolder = processInstanceIdHolder;
        this.userIdHolder = userIdHolder;
        this.variables = variables;
        this.outputTemplate = outputTemplate;
        this.faultTemplates = faultTemplates;
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
            throw new NoProcessDefinitionIdMappingException(this.getWsdlOperationName());
        }
        
        // Check that the process definition identifier exists only once in the process definitions embedded into the service unit
        int processDefinitionIdCount = 0;
        BpmnModel modelContainingProcessDefinitionId = null;
        for (final BpmnModel bpmnModel : bpmnModels) {
            for (final org.activiti.bpmn.model.Process process : bpmnModel.getProcesses()) {
                if (this.processDefinitionId.equals(process.getId())) {
                    processDefinitionIdCount++;
                    modelContainingProcessDefinitionId = bpmnModel;
                }
            }
        }
        if (processDefinitionIdCount == 0) {
            throw new ProcessDefinitionIdNotFoundInModelException(this.wsdlOperationName, this.processDefinitionId);
        } else if (processDefinitionIdCount > 1) {
            throw new ProcessDefinitionIdDuplicatedInModelException(this.wsdlOperationName, this.processDefinitionId);
        }

        // The action identifier is required
        if (this.actionId == null || this.actionId.trim().isEmpty()) {
            throw new NoActionIdMappingException(this.getWsdlOperationName());
        }

        // The mapping defining the user id is required
        if (this.userIdHolder == null) {
            throw new NoUserIdMappingException(this.wsdlOperationName);
        }
        
        // The mapping defining the output XSLT style-sheet is required
        if (this.outputTemplate == null) {
            throw new NoOutputMappingException(wsdlOperationName);
        }

        this.doAnnotationCoherenceCheck(modelContainingProcessDefinitionId);

        // Check the existence of declared variables into the process definition
        for (final String variableName : this.variables.keySet()) {
            if (!this.variableTypes.containsKey(variableName)) {
                throw new VariableNotFoundInModelException(this.wsdlOperationName, variableName,
                        this.processDefinitionId);
            }
        }

        // Check that the all required variables of the process definition are mapped
        for (final Entry<String, FormProperty> entry : this.variableTypes.entrySet()) {
            final String variableName = entry.getKey();
            if (entry.getValue().isRequired() && !this.variables.containsKey(variableName)) {
                throw new RequiredVariableMissingException(this.wsdlOperationName, variableName);
            }
        }

        // Check supported fault mapping
        final List<String> mappedExceptionNames = this.getMappedExceptionNames();
        for (final String faultName : this.getFaultTemplates().keySet()) {
            if (!mappedExceptionNames.contains(faultName)) {
                throw new UnsupportedMappedExceptionNameException(this.wsdlOperationName, faultName);
            }
        }
    }

    private List<String> getMappedExceptionNames() {
        final List<String> mappedExceptionNames = new ArrayList<String>();
        this.addMappedExceptionNames(mappedExceptionNames);
        return mappedExceptionNames;
    }

    protected abstract void addMappedExceptionNames(final List<String> mappedExceptionNames);

    /**
     * <p>
     * Entry point to:
     * <ul>
     * <li>extend checks about annotation coherence,</li>
     * <li>and to populate variable types.</li>
     * </ul>
     * </p>
     * 
     * @throws model
     *             The model of the process definition
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    public abstract void doAnnotationCoherenceCheck(final BpmnModel model)
            throws InvalidAnnotationForOperationException;

    /**
     * @return The WSDL operation containing the current annotations
     */
    public String getWsdlOperationName() {
        return this.wsdlOperationName;
    }

    /**
     * @return The BPMN process definition identifier associated to the BPMN operation
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

    /**
     * @return The task identifier on which the action must be realize on the BPMN process side
     */
    public String getActionId() {
        return this.actionId;
    }

    /**
     * @return The action to realize on the BPMN process side
     */
    public abstract String getAction();

    /**
     * @return The placeholder of BPMN process instance identifier associated to the BPMN operation.
     */
    public XPathExpression getProcessInstanceIdHolder() {
        return this.processInstanceIdHolder;
    }

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
     * @return The output XSLT style-sheet compiled
     */
    public Templates getOutputTemplate() {
        return this.outputTemplate;
    }

    /**
     * @return The XSLT style-sheet compiled associated to WSDL faults
     */
    public Map<String, Templates> getFaultTemplates() {
        return this.faultTemplates;
    }
}
