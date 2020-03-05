/**
 * Copyright (c) 2014-2020 Linagora
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
package org.ow2.petals.flowable;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import javax.xml.namespace.QName;

import org.flowable.bpmn.model.BpmnModel;
import org.flowable.bpmn.model.Process;
import org.flowable.engine.RepositoryService;
import org.flowable.engine.repository.Deployment;
import org.flowable.engine.repository.DeploymentBuilder;
import org.flowable.engine.repository.ProcessDefinition;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.jbidescriptor.generated.Provides;
import org.ow2.petals.component.framework.jbidescriptor.generated.Services;
import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.se.ServiceEngineServiceUnitManager;
import org.ow2.petals.component.framework.su.ServiceUnitDataHandler;
import org.ow2.petals.component.framework.util.ServiceEndpointOperationKey;
import org.ow2.petals.flowable.exception.NoAnnotatedOperationDeclarationException;
import org.ow2.petals.flowable.exception.ProcessDefinitionDeclarationException;
import org.ow2.petals.flowable.incoming.operation.CompleteUserTaskOperation;
import org.ow2.petals.flowable.incoming.operation.EmbeddedProcessDefinition;
import org.ow2.petals.flowable.incoming.operation.FlowableOperation;
import org.ow2.petals.flowable.incoming.operation.IntermediateMessageCatchEventOperation;
import org.ow2.petals.flowable.incoming.operation.MessageStartEventOperation;
import org.ow2.petals.flowable.incoming.operation.NoneStartEventOperation;
import org.ow2.petals.flowable.incoming.operation.annotated.AnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.annotated.AnnotatedWsdlParser;
import org.ow2.petals.flowable.incoming.operation.annotated.CompleteUserTaskAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.annotated.IntermediateMessageCatchEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.annotated.MessageStartEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.annotated.NoneStartEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.UnsupportedActionException;
import org.ow2.petals.flowable.utils.BpmnReader;
import org.ow2.petals.flowable.utils.InternalBPMNDefinitionURIBuilder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * @author Bertrand ESCUDIE - Linagora
 */
public class FlowableSuManager extends ServiceEngineServiceUnitManager {

    /**
     * Activation flag of the BPMN validation on process deployments into the Flowable engine
     */
    private boolean enableFlowableBpmnValidation;

    /**
     * An UUID generator.
     */
    private final SimpleUUIDGenerator simpleUUIDGenerator;

    /**
     * Default constructor.
     * 
     * @param component
     *            the Flowable component
     * @param simpleUUIDGenerator
     *            An UUID generator
     */
    public FlowableSuManager(final AbstractServiceEngine component, final SimpleUUIDGenerator simpleUUIDGenerator) {
        super(component);
        this.simpleUUIDGenerator = simpleUUIDGenerator;
    }

    /**
     * 
     * @param enableFlowableBpmnValidation
     *            Activation flag of the BPMN validation on process deployments into the Flowable engine
     */
    public void setEnableFlowableBpmnValidation(boolean enableFlowableBpmnValidation) {
        this.enableFlowableBpmnValidation = enableFlowableBpmnValidation;
    }

    @Override
    protected void doDeploy(final ServiceUnitDataHandler suDH) throws PEtALSCDKException {
        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("Start FlowableSuManager.doDeploy(SU = " + suDH.getName() + ")");
        }

        final Jbi jbiDescriptor = suDH.getDescriptor();

        // Check the JBI descriptor
        if (jbiDescriptor == null || jbiDescriptor.getServices() == null) {
            throw new PEtALSCDKException("Invalid JBI descriptor: it does not contain a 'services' section.");
        }
        
        String tenantId = extractTenantId(jbiDescriptor.getServices());
        if (tenantId == null) {
            // TODO: Improve the default value declaration
            tenantId = "myTenant"; // default value
        }

        String categoryId = extractCategoryId(jbiDescriptor.getServices());
        if (categoryId == null) {
            // TODO: Improve the default value declaration
            categoryId = "myCategory"; // default value
        }

        // Read BPMN models from files of the service-unit
        final BpmnReader bpmnReader = new BpmnReader(jbiDescriptor.getServices(), suDH.getInstallRoot(), this.logger);
        final Map<String, EmbeddedProcessDefinition> embeddedBpmnModels = bpmnReader.readBpmnModels();


        final List<Provides> provides = jbiDescriptor.getServices().getProvides();
        if (provides == null || provides.isEmpty()) {
            this.logger.info(String.format(
                    "No provider defined in the service unit '%s'. Perhaps it's the deployment of a process used only through call activity steps.",
                    suDH.getName()));

            // Deploy processes from the BPMN models into the BPMN engine
            this.deployBpmnModels(embeddedBpmnModels, tenantId, categoryId, null, suDH);

        } else if (provides.size() > 1) {
            // Check that there is only one Provides section in the SU
            throw new PEtALSCDKException("Invalid JBI descriptor: it must not have more than one 'provides' section.");
        } else {

            final Provides theOnlyOneProvide = provides.get(0);
            if (theOnlyOneProvide == null) {
                throw new PEtALSCDKException("Invalid JBI descriptor: the 'provides' section is invalid.");
            }

            // Create processing operations
            final List<BpmnModel> bpmnModels = new ArrayList<>(embeddedBpmnModels.size());
            for (final EmbeddedProcessDefinition embeddedBpmnModel : embeddedBpmnModels.values()) {
                bpmnModels.add(embeddedBpmnModel.getModel());
            }

            final List<FlowableOperation> operations = this.createProcessingOperations(suDH.getInstallRoot(),
                    suDH.getEndpointDescription(theOnlyOneProvide), bpmnModels, tenantId);

            // Deploy processes from the BPMN models into the BPMN engine
            this.deployBpmnModels(embeddedBpmnModels, tenantId, categoryId, operations, suDH);

            // Enable processing operations
            final String edptName = theOnlyOneProvide.getEndpointName();
            final QName serviceName = theOnlyOneProvide.getServiceName();
            for (final FlowableOperation operation : operations) {
                // Store the FlowableOperation in the map with the corresponding end-point
                final ServiceEndpointOperationKey eptAndOperation = new ServiceEndpointOperationKey(serviceName,
                        edptName, operation.getWsdlOperation());
                getComponent().registerFlowableService(eptAndOperation, operation);
            }
            getComponent().logEptOperationToFlowableOperation(this.logger, Level.FINEST);
        }

        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("End FlowableSuManager.doDeploy()");
        }
    }
    
    /**
     * Extracts the tenant identifier from the JBI descriptor definition
     * 
     * @param services
     *            Extra parameters of the section 'services'
     * @return the tenant id or {@code null} if not found.
     */
    private static String extractTenantId(final Services services) {
        assert services != null;
        
        final List<Element> extensions = services.getAnyOrAny();
        for (final Element e : extensions) {
            assert e != null;

            if (FlowableSEConstants.TENANT_ID.equals(e.getLocalName())) {
                return e.getTextContent();
            }
        }

        // Here no param 'tenant id' was found
        return null;
    }

    /**
     * Extracts the category identifier from the JBI descriptor definition
     * 
     * @param services
     *            Extra parameters of the section 'services'
     * @return the category id or {@code null} if not found.
     */
    private static String extractCategoryId(final Services services) {
        assert services != null;

        final List<Element> extensions = services.getAnyOrAny();
        for (final Element e : extensions) {
            assert e != null;

            if (FlowableSEConstants.CATEGORY_ID.equals(e.getLocalName())) {
                return e.getTextContent();
            }
        }

        // Here no param 'category id' was found
        return null;
    }

    @Override
    protected void doStart(final ServiceUnitDataHandler suDH) throws PEtALSCDKException {
        this.logger.fine("Start FlowableSuManager.doStart(SU =" + suDH.getName() + ")");

        // TODO Manage the process suspension State be careful of multi-SU deployment for the same process

        this.logger.fine("End FlowableSuManager.doStart()");
    }

    @Override
    protected void doStop(final ServiceUnitDataHandler suDH) throws PEtALSCDKException {
        this.logger.fine("Start FlowableSuManager.doStop(SU =" + suDH.getName() + ")");

        // TODO Manage the process suspension State: be careful of multi SU deployement for the same process

        this.logger.fine("End FlowableSuManager.doStop()");
    }

    @Override
    protected void doUndeploy(final ServiceUnitDataHandler suDH) throws PEtALSCDKException {
        this.logger.fine("Start FlowableSuManager.doUndeploy(SU =" + suDH.getName() + ")");
        try {
            final List<Provides> provides = suDH.getDescriptor().getServices().getProvides();
            if (provides != null && !provides.isEmpty()) {
                final String edptName = suDH.getDescriptor().getServices().getProvides().iterator().next()
                        .getEndpointName();
                // Remove the FlowableOperation in the map with the corresponding end-point
                getComponent().removeFlowableService(edptName);
            }
            

            /**
             * // Get the operation Name of the end point ServiceUnitDataHandler suDataHandler
             * =this.getSUDataHandlerForEndpoint(serviceEndpoint); List<QName> operations =
             * suDataHandler.getEndpointOperationsList(serviceEndpoint);
             * 
             * for ( QName operation : operations) { eptAndOperation = new
             * EptAndOperation(edptName,operation.getLocalPart()); // Remove the FlowableOperation in the map with the
             * corresponding end-point and Operation ((FlowableSE)
             * this.component).removeFlowableOperation(eptAndOperation); logger.info("*** ept: "+
             * eptAndOperation.getEptName() + " operation : "+ eptAndOperation.getOperationName()); logger.info(
             * "          is removed from MAP eptOperationToFlowableOperation" ); }
             */
            // TODO Manage the undeployement of the process: be careful of multi SU deployement for the same process
            
        } finally {
            this.logger.fine("End FlowableSuManager.doUndeploy()");
        }
    }

    /**
     * <p>
     * Deploys the embedded process definition into the BPMN engine.
     * </p>
     * <p>
     * Notes:
     * <ul>
     * <li>the operations defined into the WSDL are updated about the deployed process instance identifier,</li>
     * <li>a process definition is deployed if it is not already deployed with the same version,</li>
     * <li>process deployment is characterized in Flowable Database by processName / tenantId / categoryId / version,
     * </li>
     * <li>tenantId allows to have several instance of the same process model in different contexts: different owner,
     * assignee, group ....</li>
     * <li>categoryId allows to manage process lifeCycle: Dev, PreProd, Prod ...</li>
     * <li>version allows to manage several versions.</li>
     * </ul>
     * </p>
     * 
     * @param embeddedBpmnModels
     *            The embedded process definitions
     * @param tenantId
     *            The tenant identifier of the embedded process. Not {@code null}.
     * @param categoryId
     *            The category identifier of the embedded process. Not {@code null}.
     * @param operations
     *            The list of operations described into the WSDL
     * @param suDH
     *            Current service unit data handler
     * @throws PEtALSCDKException
     */
    private void deployBpmnModels(final Map<String, EmbeddedProcessDefinition> embeddedBpmnModels,
            final String tenantId, final String categoryId, final List<FlowableOperation> operations,
            final ServiceUnitDataHandler suDH) throws PEtALSCDKException {

        assert tenantId != null;
        assert categoryId != null;

        final Iterator<EmbeddedProcessDefinition> iterator = embeddedBpmnModels.values().iterator();
        while (iterator.hasNext()) {
            final EmbeddedProcessDefinition process = iterator.next();
            
            // Check that the BPMN definition is not already deployed. If it exists do not deploy it again.
            // This allow to deploy the same SU (process.bpmn20.xml):
            // - on several petals-se-flowable for high availability, ie. to create several service endpoints for the
            // same process/tenantId/categoryId/version on
            // different Petals ESB container,
            // - to redeploy the SU after few modifications not about the process definition (example: timeout for
            // service consumer, ...),
            // - or to restart the Petals container (SUs are redeployed on Petals container start-up)
            
            // NOTE: A BPMN definition can contain several BPMN processes. So, checking that a BPMN definition is
            // not already deployed is:
            // - try to retrieve process definitions associated to the BPMN definition for the given version
            // - if not process definition is found, we can deploy the BPMN definition
            // - we compare retrieved process definition with the ones included into the BPMN definition file
            // - if they are the same, the BPMN definition is not deployed,
            // - if they are not the same (new process definitions exist into the BPMN definition file or few was
            // removed), a warning is logged about the BPMN definition is not the same for the same version. The BPMN
            // definition is not deployed.

            final RepositoryService repositoryService = getComponent().getProcessEngine().getRepositoryService();
            
            final List<ProcessDefinition> existingProcDefs = repositoryService.createProcessDefinitionQuery()
                    .processDefinitionResourceName(InternalBPMNDefinitionURIBuilder
                            .buildURI(suDH.getName(), process.getProcessFileName()).toASCIIString())
                    // TODO: We should filter by category, but Flowable seems to not manage correctly this attribute. To
                    // investigate where is the problem.
                    // .processDefinitionCategory(categoryId)
                    .processDefinitionTenantId(tenantId).processDefinitionVersion(process.getVersion()).list();
            
            final boolean mustDeployBpmnDef;
            final List<Process> currentProcDefs = new ArrayList<>(process.getModel().getProcesses());
            if (currentProcDefs.isEmpty()) {
                this.logger.warning(String.format(
                        "No process definition exists in the BPMN definition file '%s'. BPMN definition file not deployed !",
                        process.getProcessFileName()));
                mustDeployBpmnDef = false;
            } else {
                if (existingProcDefs.isEmpty()) {
                    // No process definition found associated to the BPMN file, we can deploy it
                    mustDeployBpmnDef = true;
                } else {
                    // Process definitions found associated to the BPMN file, we does not deploy it
                    mustDeployBpmnDef = false;

                    final Iterator<Process> itCurrentProcDefs = currentProcDefs.iterator();
                    // We duplicate 'existingProcDefs' because it is reused later and here we need to remove elements
                    final List<ProcessDefinition> tmpExistingProcDefs = new ArrayList<>(existingProcDefs);
                    final Iterator<ProcessDefinition> itTmpExistingProcDefs = tmpExistingProcDefs.iterator();
                    while (itCurrentProcDefs.hasNext()) {
                        while (itTmpExistingProcDefs.hasNext())
                            if (itTmpExistingProcDefs.next().getKey().equals(itCurrentProcDefs.next().getId())) {
                                itCurrentProcDefs.remove();
                                itTmpExistingProcDefs.remove();
                            }
                    }
                    if (currentProcDefs.isEmpty() && tmpExistingProcDefs.isEmpty()) {
                        // BPMN definition already deployed contains the same process definitions than the one to deploy
                        this.logger.info(String.format(
                                "The BPMN definition file '%s' is already deployed for the version %d. BPMN definition file not deployed again !",
                                process.getProcessFileName(), process.getVersion()));
                    } else {
                        this.logger.warning(String.format(
                                "The BPMN definition file '%s' is already deployed for the version %d and it contains different process definitions than the one already deployed. BPMN definition file not deployed again !",
                                process.getProcessFileName(), process.getVersion()));
                    }
                }

            }

            final List<ProcessDefinition> processDefinitions;
            if (mustDeployBpmnDef) {
                final DeploymentBuilder db = repositoryService.createDeployment();

                // Characterize the deployment with processFileName / tenantId / categoryId
                db.name("Process read from: " + process.getProcessFileName());
                db.tenantId(tenantId);
                db.category(categoryId);
                // We localize the BPMN definition file using an URI of a internal format
                final URI bpmnDefinitionInternalURI = InternalBPMNDefinitionURIBuilder.buildURI(suDH.getName(),
                        process.getProcessFileName());
                try {
                    final FileInputStream bpmnInputFile = new FileInputStream(
                            new File(suDH.getInstallRoot(), process.getProcessFileName()));
                    db.addInputStream(bpmnDefinitionInternalURI.toASCIIString(), bpmnInputFile);
                } catch (final FileNotFoundException e) {
                    throw new PEtALSCDKException(e);
                }

                if (!this.enableFlowableBpmnValidation) {
                    db.disableBpmnValidation();
                    db.disableSchemaValidation();
                }

                // TODO Manage the process suspension State be careful of multi SU deployement for the same
                // process

                // Do not use db.enableDuplicateFiltering() with management of tenantId and CategoryId
                final Deployment deployment = db.deploy();
                processDefinitions = repositoryService.createProcessDefinitionQuery().deploymentId(deployment.getId())
                        .list();

                if (this.logger.isLoggable(Level.INFO)) {
                    this.logger.info(String.format("The BPMN process '%s' version %d is succesfully deployed:",
                            process.getProcessFileName(), process.getVersion()));
                }

            } else {
                processDefinitions = existingProcDefs;

                if (this.logger.isLoggable(Level.INFO)) {
                    this.logger.info(String.format("The BPMN process '%s' version %d is already deployed:",
                            process.getProcessFileName(), process.getVersion()));
                }
            }

            if (this.logger.isLoggable(Level.INFO)) {
                for (final ProcessDefinition processDefinition : processDefinitions) {
                    this.logProcessDefinition(processDefinition);
                }
            }

            // For each operation we must set its deployed process instance identifier
            if (operations != null) {
                for (final ProcessDefinition processDefinition : processDefinitions) {
                    for (final FlowableOperation operation : operations) {
                        if (processDefinition.getKey().equals(operation.getProcessDefinitionId())) {
                            operation.setDeployedProcessDefinitionId(processDefinition.getId());
                        }
                    }
                }
            }
        }
    }

    private void logProcessDefinition(final ProcessDefinition processDefinition) {
        this.logger.fine("\t- Id            = " + processDefinition.getId());
        this.logger.fine("\t\t- Category      = " + processDefinition.getCategory());
        this.logger.fine("\t\t- Name          = " + processDefinition.getName());
        this.logger.fine("\t\t- Key           = " + processDefinition.getKey());
        this.logger.fine("\t\t- Version       = " + processDefinition.getVersion());
        this.logger.fine("\t\t- Deployemnt Id = " + processDefinition.getDeploymentId());
        this.logger.fine("\t\t- ResourceName  = " + processDefinition.getResourceName());
        this.logger.fine("\t\t- TenantId      = " + processDefinition.getTenantId());
    }

    /**
     * Create the processing operations ({@link FlowableOperation} reading annotations of the WSDL
     * 
     * @param suRootPath
     *            The root directory of the service unit. Not {@code null}.
     * @param wsdlDocument
     *            The WSDL of the service provider. Not {@code null}.
     * @param bpmnModels
     *            The BPMN models embedded into the service unit. Not {@code null}.
     * @param tenantId
     *            Tenant identifier in which the process definition is deployed. Not {@code null}.
     * @return The list of {@link FlowableOperation} created from WSDL
     * @throws ProcessDefinitionDeclarationException
     *             An error was detected about annotations
     */
    private List<FlowableOperation> createProcessingOperations(final String suRootPath, final Document wsdlDocument,
            final List<BpmnModel> bpmnModels, final String tenantId) throws ProcessDefinitionDeclarationException {

        assert suRootPath != null;
        assert wsdlDocument != null;
        assert bpmnModels != null;
        assert tenantId != null;

        final AnnotatedWsdlParser annotatedWdslParser = new AnnotatedWsdlParser(tenantId, this.logger);
        final List<AnnotatedOperation> annotatedOperations = annotatedWdslParser.parse(wsdlDocument, bpmnModels,
                suRootPath);
        // Log all WSDL errors before to process each annotated operations
        if (this.logger.isLoggable(Level.WARNING)) {
            for (final InvalidAnnotationException encounteredError : annotatedWdslParser.getEncounteredErrors()) {
                this.logger.warning(encounteredError.getMessage());
            }
        }
        if (annotatedOperations.isEmpty()) {
            // No annotated operation was correctly read from the WSDL, or no annotated operation is declared in the
            // WSDL
            throw new NoAnnotatedOperationDeclarationException();
        }

        final List<FlowableOperation> operations = new ArrayList<>(annotatedOperations.size());
        for (final AnnotatedOperation annotatedOperation : annotatedOperations) {

            // create the right FlowableOperation according to the bpmnActionType
            operations.add(this.createProcessingOperation(annotatedOperation));
        }

        return operations;
    }

    /**
     * Create the {@link FlowableOperation} according to the {@link AnnotatedOperation}.
     * 
     * @param annotatedOperation
     * @return
     * @throws ProcessDefinitionDeclarationException
     */
    private FlowableOperation createProcessingOperation(final AnnotatedOperation annotatedOperation)
            throws ProcessDefinitionDeclarationException {

        final QName wsdlOperation = annotatedOperation.getWsdlOperation();
        this.logger.fine("Processing WSDL annotated operation: " + wsdlOperation);

        final FlowableSE component = this.getComponent();
        if (annotatedOperation instanceof NoneStartEventAnnotatedOperation) {
            return new NoneStartEventOperation((NoneStartEventAnnotatedOperation) annotatedOperation,
                    component.getProcessEngine().getIdentityService(), component.getProcessEngine().getRuntimeService(),
                    component.getProcessEngine().getHistoryService(), this.simpleUUIDGenerator,
                    component.getPlaceHolders(), this.logger);
        } else if (annotatedOperation instanceof MessageStartEventAnnotatedOperation) {
            return new MessageStartEventOperation((MessageStartEventAnnotatedOperation) annotatedOperation,
                    component.getProcessEngine().getIdentityService(), component.getProcessEngine().getRuntimeService(),
                    component.getProcessEngine().getHistoryService(), this.simpleUUIDGenerator,
                    component.getPlaceHolders(), this.logger);
        } else if (annotatedOperation instanceof CompleteUserTaskAnnotatedOperation) {
            return new CompleteUserTaskOperation((CompleteUserTaskAnnotatedOperation) annotatedOperation,
                    component.getProcessEngine().getTaskService(), component.getProcessEngine().getIdentityService(),
                    component.getProcessEngine().getHistoryService(), component.getProcessEngine().getRuntimeService(),
                    this.logger);
        } else if (annotatedOperation instanceof IntermediateMessageCatchEventAnnotatedOperation) {
            return new IntermediateMessageCatchEventOperation(
                    (IntermediateMessageCatchEventAnnotatedOperation) annotatedOperation,
                    component.getProcessEngine().getRuntimeService(), component.getProcessEngine().getHistoryService(),
                    this.logger);
        } else {
            // This case is a bug case, as the annotated operation is known by the parser, it must be supported
            // here.
            throw new ProcessDefinitionDeclarationException(
                    new UnsupportedActionException(wsdlOperation, annotatedOperation.getClass().getSimpleName()));
        }

    }

    @Override
    protected FlowableSE getComponent() {
        return (FlowableSE) super.getComponent();
    }

}
