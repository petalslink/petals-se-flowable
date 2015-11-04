/**
 * Copyright (c) 2014-2015 Linagora
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
package org.ow2.petals.activitibpmn;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import javax.xml.namespace.QName;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.converter.util.InputStreamProvider;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.engine.RepositoryService;
import org.activiti.engine.impl.util.io.InputStreamSource;
import org.activiti.engine.repository.Deployment;
import org.activiti.engine.repository.DeploymentBuilder;
import org.activiti.engine.repository.ProcessDefinition;
import org.ow2.petals.activitibpmn.exception.IncoherentProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.InvalidVersionDeclaredException;
import org.ow2.petals.activitibpmn.exception.NoAnnotatedOperationDeclarationException;
import org.ow2.petals.activitibpmn.exception.NoProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.ProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.UnexistingProcessFileException;
import org.ow2.petals.activitibpmn.incoming.operation.ActivitiOperation;
import org.ow2.petals.activitibpmn.incoming.operation.CompleteUserTaskOperation;
import org.ow2.petals.activitibpmn.incoming.operation.EmbeddedProcessDefinition;
import org.ow2.petals.activitibpmn.incoming.operation.StartEventOperation;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.AnnotatedWsdlParser;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.CompleteUserTaskAnnotatedOperation;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.StartEventAnnotatedOperation;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.UnsupportedActionException;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.api.configuration.SuConfigurationParameters;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.jbidescriptor.generated.Provides;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.ow2.petals.component.framework.su.ServiceUnitDataHandler;
import org.ow2.petals.component.framework.util.EndpointOperationKey;
import org.w3c.dom.Document;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;


/**
 * @author Bertrand ESCUDIE - Linagora
 */
public class ActivitiSuManager extends AbstractServiceUnitManager {

    /**
     * Activation flag of the BPMN validation on process deployments into the Activiti engine
     */
    private boolean enableActivitiBpmnValidation;

    /**
     * An UUID generator.
     */
    private final SimpleUUIDGenerator simpleUUIDGenerator;
	
    /**
     * Default constructor.
     * 
     * @param component
     *            the ACTIVITI component
     * @param simpleUUIDGenerator
     *            An UUID generator
     */
    public ActivitiSuManager(final AbstractComponent component, final SimpleUUIDGenerator simpleUUIDGenerator) {
        super(component);
        this.simpleUUIDGenerator = simpleUUIDGenerator;
    }

    /**
     * 
     * @param enableActivitiBpmnValidation
     *            Activation flag of the BPMN validation on process deployments into the Activiti engine
     */
    public void setEnableActivitiBpmnValidation(boolean enableActivitiBpmnValidation) {
        this.enableActivitiBpmnValidation = enableActivitiBpmnValidation;
    }

    @Override
    protected void doDeploy(final String serviceUnitName, final String suRootPath, final Jbi jbiDescriptor)
            throws PEtALSCDKException {
        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("Start ActivitiSuManager.doDeploy(SU =" + serviceUnitName + ")");
        }

		// Check the JBI descriptor
		if( jbiDescriptor == null || jbiDescriptor.getServices() == null
                || jbiDescriptor.getServices().getProvides() == null
                || jbiDescriptor.getServices().getProvides().size() == 0) {
			throw new PEtALSCDKException( "Invalid JBI descriptor: it does not contain a 'provides' section." );
        }

		// Check that there is only one Provides section in the SU
        if (jbiDescriptor.getServices().getProvides().size() != 1) {
			throw new PEtALSCDKException( "Invalid JBI descriptor: it must not have more than one 'provides' section." );
        }

        // Get the provides
        final Provides provides = jbiDescriptor.getServices().getProvides().get(0);
        if (provides == null) {
			throw new PEtALSCDKException( "Invalid JBI descriptor: the 'provides' section is invalid." );
        }

        // Get the SU Data handler
        final ServiceUnitDataHandler suDataHandler = this.getSUDataHandler(serviceUnitName);
        if (suDataHandler == null) {
            throw new PEtALSCDKException(
                    "Error while processing the JBI descriptor in the component. The SU data handler was null.");
        }

        // Get the extension configuration for the Activiti process(es) to be deployed from the SU jbi.xml
        final SuConfigurationParameters extensions = suDataHandler.getConfigurationExtensions(provides);
        if (extensions == null) {
            throw new PEtALSCDKException("Invalid JBI descriptor: it does not contain any component extension.");
        }

        // Read BPMN models from files of the service-unit
        final Map<String, EmbeddedProcessDefinition> embeddedBpmnModels = this.readBpmnModels(extensions,
                suDataHandler.getInstallRoot());

        // Create processing operations
        final List<BpmnModel> bpmnModels = new ArrayList<BpmnModel>(embeddedBpmnModels.size());
        for (final EmbeddedProcessDefinition embeddedBpmnModel : embeddedBpmnModels.values()) {
            bpmnModels.add(embeddedBpmnModel.getModel());
        }
        final List<ActivitiOperation> operations = this.createProcessingOperations(serviceUnitName, bpmnModels,
                suDataHandler.getInstallRoot());
        
        // Deploy processes from the BPMN models into the BPMN engine
        this.deployBpmnModels(embeddedBpmnModels, operations, suRootPath);
        
        // Enable processing operations
        final String edptName = provides.getEndpointName();
        final QName interfaceName = provides.getInterfaceName();
        for (final ActivitiOperation operation : operations) {
            // Store the ActivitiOperation in the map with the corresponding end-point
            final EndpointOperationKey eptAndOperation = new EndpointOperationKey(edptName, interfaceName,
                    operation.getWsdlOperation());
            ((ActivitiSE) this.component).registerActivitiService(eptAndOperation, operation);
        }
        ((ActivitiSE) this.component).logEptOperationToActivitiOperation(this.logger, Level.FINEST);
        
        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("End ActivitiSuManager.doDeploy()");
		}
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doStart(final String serviceUnitName) throws PEtALSCDKException {
        this.logger.fine("Start ActivitiSuManager.doStart(SU =" + serviceUnitName + ")");

		// TODO Manage the process suspension State be careful of multi-SU deployment for the same process
    	
        this.logger.fine("End ActivitiSuManager.doStart()");
    }


    /**
     * {@inheritDoc}
     */
    @Override
    protected void doStop(final String serviceUnitName) throws PEtALSCDKException {
        this.logger.fine("Start ActivitiSuManager.doStop(SU =" + serviceUnitName + ")");

		// TODO Manage the process suspension State: be careful of multi SU deployement for the same process

        this.logger.fine("End ActivitiSuManager.doStop()");
    }


    /**
     * {@inheritDoc}
     */
    @Override
    protected void doUndeploy(final String serviceUnitName) throws PEtALSCDKException {
        this.logger.fine("Start ActivitiSuManager.doUndeploy(SU =" + serviceUnitName + ")");
        try {
            final String edptName = this.getSUDataHandler(serviceUnitName).getDescriptor().getServices().getProvides()
                    .iterator().next().getEndpointName();
            // Remove the ActivitiOperation in the map with the corresponding end-point
            ((ActivitiSE) this.component).removeActivitiService(edptName);

            /**
             * // Get the operation Name of the end point ServiceUnitDataHandler suDataHandler
             * =this.getSUDataHandlerForEndpoint(serviceEndpoint); List<QName> operations =
             * suDataHandler.getEndpointOperationsList(serviceEndpoint);
             * 
             * for ( QName operation : operations) { eptAndOperation = new
             * EptAndOperation(edptName,operation.getLocalPart()); // Remove the ActivitiOperation in the map with the
             * corresponding end-point and Operation ((ActivitiSE)
             * this.component).removeActivitiOperation(eptAndOperation); logger.info("*** ept: "+
             * eptAndOperation.getEptName() + " operation : "+ eptAndOperation.getOperationName());
             * logger.info("          is removed from MAP eptOperationToActivitiOperation" ); }
             */
            // TODO Manage the undeployement of the process: be careful of multi SU deployement for the same process
        } finally {
            this.logger.fine("End ActivitiSuManager.doUndeploy()");
        }
    }

    /**
     * <p>
     * Read embedded BPMN models of the SU from their raw information
     * </p>
     * 
     * @return The map of embedded process definition containing. The map key is the process file name.
     * @param extensions
     *            The configuration extensions of the service unit. Not <code>null</code>.
     * @param suRootPath
     *            The root directory of the service unit. Not <code>null</code>.
     * @throws ProcessDefinitionDeclarationException
     *             A raw data of the SU JBI descriptor is invalid
     */
    private Map<String, EmbeddedProcessDefinition> readBpmnModels(final SuConfigurationParameters extensions,
            final String suRootPath) throws ProcessDefinitionDeclarationException {

        assert extensions != null;
        assert suRootPath != null;

        String tenantId = extensions.get(ActivitiSEConstants.TENANT_ID);
        if (tenantId == null) {
            // TODO: Improve the default value declaration
            tenantId = "myTenant"; // default value
        }

        String categoryId = extensions.get(ActivitiSEConstants.CATEGORY_ID);
        if (categoryId == null) {
            // TODO: Improve the default value declaration
            categoryId = "myCategory"; // default value
        }

        final Map<String, EmbeddedProcessDefinition> bpmnModels = new HashMap<String, EmbeddedProcessDefinition>();

        String processFileName = extensions.get(ActivitiSEConstants.PROCESS_FILE);
        String versionStr = extensions.get(ActivitiSEConstants.VERSION);
        if (processFileName == null && versionStr == null) {
            // The SU does not contain an unique process definition, perhaps several process definitions

            // TODO: Multi-process definitions should be reviewed because a wrapper tag is missing: The right writing
            // style would be:
            //    <process-definition>
            //       <process_file>...</process_file>
            //       <version>...</version>
            //    </process-definition>
            // But the CDK provides a mechanism using increment.
            int nbProcesses = 1;
            do {
                processFileName = extensions.get(ActivitiSEConstants.PROCESS_FILE + nbProcesses);
                versionStr = extensions.get(ActivitiSEConstants.VERSION + nbProcesses);
                if (nbProcesses == 1 && processFileName == null && versionStr == null) {
                    throw new NoProcessDefinitionDeclarationException();
                } else if (processFileName != null && versionStr != null) {
                    bpmnModels.put(processFileName,
                            this.readBpmnModel(processFileName, versionStr, tenantId, categoryId, suRootPath));
                    nbProcesses++;
                } else if ((processFileName != null && versionStr == null)
                        || (processFileName == null && versionStr != null)) {
                    throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
                } else {
                    // Here, processFileName == null and versionStr == null, and at least one process definition was
                    // previously read, so we have nothing to do, we will exit the loop
                }
            } while (processFileName != null && versionStr != null);
        } else if (processFileName != null && versionStr != null) {
            // One process description
            bpmnModels.put(processFileName,
                    this.readBpmnModel(processFileName, versionStr, tenantId, categoryId, suRootPath));
        } else {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        }

        return bpmnModels;
    }

    /**
     * <p>
     * Read an embedded BPMN model of the SU from its raw information.
     * </p>
     * 
     * @param processFileName
     *            The file name of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @param versionStr
     *            The version of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @param tenantId
     *            The tenant identifier of the process, as raw data read from the SU JBI descriptor. Not
     *            <code>null</code>.
     * @param categoryId
     *            The category identifier of the process, as raw data read from the SU JBI descriptor. Not
     *            <code>null</code>.
     * @param suRootPath
     *            <code>processFileName</code> is relative to the service unit root path. Not <code>null</code>.
     * @throws ProcessDefinitionDeclarationException
     *             A raw data of the SU JBI descriptor is invalid
     */
    private EmbeddedProcessDefinition readBpmnModel(final String processFileName, final String versionStr,
            final String tenantId, final String categoryId, final String suRootPath)
            throws ProcessDefinitionDeclarationException {

        if (processFileName != null && processFileName.trim().isEmpty()) {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        }

        final int version;
        if (versionStr != null && versionStr.trim().isEmpty()) {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        } else {
            try {
                version = Integer.parseInt(versionStr);
            } catch (final NumberFormatException e) {
                throw new InvalidVersionDeclaredException(processFileName, versionStr);
            }
        }

        // Read the BPMN model
        final File processFile = new File(suRootPath, processFileName);
        try {
            final FileInputStream bpmnInputFile = new FileInputStream(processFile);
            try {
                final InputStreamProvider bpmnInputStreamSource = new InputStreamSource(bpmnInputFile);

                // add BPMN Model in order to change the category in the Process definition
                // that is indeed derived from the targetNameSpace of the bpmn20.xml file
                // TODO: Enable validation
                final BpmnModel bpmnModel = new BpmnXMLConverter().convertToBpmnModel(bpmnInputStreamSource, false,
                        false);

                // TODO manage the assignee according with su jbi descriptor

                if (this.logger.isLoggable(Level.FINE)) {
                    this.logger.fine("The BPMN process [file: " + processFileName + ", version: " + version
                            + "] is succesfully read");
                }

                return new EmbeddedProcessDefinition(processFileName, version, tenantId, categoryId, bpmnModel);

            } finally {
                try {
                    bpmnInputFile.close();
                } catch (final IOException e) {
                    this.logger.log(Level.WARNING, "Unable to close BPMN definition file ''. Error skiped !", e);
                }
            }

        } catch (final FileNotFoundException e) {
            throw new UnexistingProcessFileException(processFile.getAbsolutePath(), e);
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
     * <li>a process definition is deployed if it is not already deployed,</li>
     * <li>process deployment is characterized in Activiti Database by processName / tenantId / categoryId / version,</li>
     * <li>
     * tenantId allows to have several instance of the same process model in different contexts: different owner,
     * assignee, group ....</li>
     * <li>categoryId allows to manage process lifeCycle: Dev, PreProd, Prod ...</li>
     * <li>version allows to manage several versions,</li>
     * <li>he first time a process with a particular key is deployed, version 1 is assigned. For all subsequent
     * deployments of process definitions with the same key, the version will be set 1 higher then the maximum currently
     * deployed version.</li>
     * </ul>
     * </p>
     * 
     * @param embeddedBpmnModels
     *            The embedded process definitions
     * @param operations
     *            The list of operations described into the WSDL
     * @throws PEtALSCDKException
     */
    private void deployBpmnModels(final Map<String, EmbeddedProcessDefinition> embeddedBpmnModels,
            final List<ActivitiOperation> operations, final String suRootPath)
            throws PEtALSCDKException {

        final Iterator<EmbeddedProcessDefinition> iterator = embeddedBpmnModels.values().iterator();
		while (iterator.hasNext() ){
            final EmbeddedProcessDefinition process = iterator.next();
			
            // Check that the process is not already deployed. If it exists do not deploy it again then returns its
            // ProcessDefinition.
            // This allow to deploy the same SU (process.bpmn20.xml) on several petals-se-activitibpmn for high
            // availability, ie. to create several service endpoints for the same process/tenantId/categoryId/version on
            // different Petals ESB container.
            final RepositoryService repositoryService = ((ActivitiSE) this.component).getProcessEngine()
                    .getRepositoryService();
            final List<ProcessDefinition> processDefinitionSearchList = repositoryService
                    .createProcessDefinitionQuery().processDefinitionResourceName(process.getProcessFileName())
                    .processDefinitionCategory(process.getCategoryId())
                    .processDefinitionTenantId(process.getTenantId()).processDefinitionVersion(process.getVersion())
                    .list();

            final List<ProcessDefinition> processDefinitions;
            if (processDefinitionSearchList == null || processDefinitionSearchList.isEmpty()) {
                final DeploymentBuilder db = repositoryService.createDeployment();

                // Characterize the deployment with processFileName / tenantId / categoryId
                db.name("Process read from: " + process.getProcessFileName());
                db.tenantId(process.getTenantId());
                db.category(process.getCategoryId());
                // db.addBpmnModel(process.getProcessFileName(), process.getModel());
                // TODO: To remove: deployment using file and parameter 'suRootPath'
                final File processFile = new File(suRootPath, process.getProcessFileName());
                try {
                    final FileInputStream bpmnInputFile = new FileInputStream(processFile);
                    db.addInputStream(processFile.getAbsoluteFile().toURI().toString(), bpmnInputFile);
                } catch (final FileNotFoundException e) {
                    throw new PEtALSCDKException(e);
                }

                if (!this.enableActivitiBpmnValidation) {
                    db.disableBpmnValidation();
                    db.disableSchemaValidation();
                }

                // TODO Manage the process suspension State be careful of multi SU deployement for the same
                // process

                // Do not use db.enableDuplicateFiltering(); with management of tenantId and CategoryId
                final Deployment deployment = db.deploy();
                processDefinitions = repositoryService.createProcessDefinitionQuery().deploymentId(deployment.getId())
                        .list();

                if (this.logger.isLoggable(Level.INFO)) {
                    this.logger.info("The BPMN process " + process.getProcessFileName() + " version: "
                            + process.getVersion() + " is succesfully deployed.");
                }

			}
			else {
                if (this.logger.isLoggable(Level.INFO)) {
                    this.logger.info("The BPMN process: " + process.getProcessFileName() + " version: "
                            + process.getVersion() + " is already deployed");
                }
				// Set processDefinition
                processDefinitions = processDefinitionSearchList;
			}

            if (this.logger.isLoggable(Level.FINE)) {
                this.logger.fine("Process definitions deployed:");
                for (final ProcessDefinition processDefinition : processDefinitions) {
                    this.logger.fine("\t- Id            = " + processDefinition.getId());
                    this.logger.fine("\t\t- Category      = " + processDefinition.getCategory());
                    this.logger.fine("\t\t- Name          = " + processDefinition.getName());
                    this.logger.fine("\t\t- Key           = " + processDefinition.getKey());
                    this.logger.fine("\t\t- Version       = " + processDefinition.getVersion());
                    this.logger.fine("\t\t- Deployemnt Id = " + processDefinition.getDeploymentId());
                    this.logger.fine("\t\t- ResourceName  = " + processDefinition.getResourceName());
                    this.logger.fine("\t\t- TenantId      = " + processDefinition.getTenantId());
                }
            }
            
            // For each operation we must set its deployed process instance identifier
            for (final ProcessDefinition processDefinition : processDefinitions) {
                for (final ActivitiOperation operation : operations) {
                    if (processDefinition.getKey().equals(operation.getProcessDefinitionId())) {
                        operation.setDeployedProcessDefinitionId(processDefinition.getId());
                    }
                }
            }
		}
    }


    /**
     * Create the processing operations ({@link ActivitiOperation} reading annotations of the WSDL
     * 
     * @param serviceUnitName
     *            The service unit name, required to retrieve the WSDL
     * @param bpmnModels
     *            The BPMN models embedded into the service unit
     * @param suRootPath
     *            The root directory of the service unit. Not <code>null</code>.
     * @return The list of {@link ActivitiOperation} created from WSDL
     * @throws ProcessDefinitionDeclarationException
     *             An error was detected about annotations
     */
    private List<ActivitiOperation> createProcessingOperations(final String serviceUnitName,
            final List<BpmnModel> bpmnModels, final String suRootPath) throws ProcessDefinitionDeclarationException {

        final AnnotatedWsdlParser annotatedWdslParser = new AnnotatedWsdlParser(this.logger);
        
        final ServiceUnitDataHandler handler = this.getSUDataHandler(serviceUnitName);
        final Provides provides = handler.getDescriptor().getServices().getProvides().iterator().next();
        final Document wsdlDocument = handler.getEndpointDescription(provides);
        final List<AnnotatedOperation> annotatedOperations = annotatedWdslParser.parse(wsdlDocument, bpmnModels,
                suRootPath);
        // Log all WSDL errors before to process each annotated operations
        if (this.logger.isLoggable(Level.WARNING)) {
            for (final InvalidAnnotationException encounteredError : annotatedWdslParser.getEncounteredErrors()) {
                this.logger.warning(encounteredError.getMessage());
            }
        }
        if (annotatedOperations.size() == 0) {
            // No annotated operation was correctly read from the WSDL, or no annotated operation is declared in the WSDL
            throw new NoAnnotatedOperationDeclarationException();
        }

        final List<ActivitiOperation> operations = new ArrayList<ActivitiOperation>(annotatedOperations.size());
        for (final AnnotatedOperation annotatedOperation : annotatedOperations) {
            
            final QName wsdlOperation = annotatedOperation.getWsdlOperation();
            this.logger.fine("Processing WSDL annotated operation: " + wsdlOperation);

            // create the right ActivitiOperation according to the bpmnActionType
            if (annotatedOperation instanceof StartEventAnnotatedOperation) {
                operations.add(new StartEventOperation(annotatedOperation, ((ActivitiSE) this.component)
                        .getProcessEngine().getIdentityService(), ((ActivitiSE) this.component).getProcessEngine()
                        .getRuntimeService(), ((ActivitiSE) this.component).getProcessEngine().getHistoryService(),
                        this.simpleUUIDGenerator, this.logger));
            } else if (annotatedOperation instanceof CompleteUserTaskAnnotatedOperation) {
                operations.add(new CompleteUserTaskOperation(annotatedOperation, ((ActivitiSE) this.component)
                        .getProcessEngine().getTaskService(), ((ActivitiSE) this.component).getProcessEngine()
                        .getIdentityService(), ((ActivitiSE) this.component).getProcessEngine().getHistoryService(),
                        ((ActivitiSE) this.component).getProcessEngine().getRuntimeService(),
                        this.logger));
            } else {
                // This case is a bug case, as the annotated operation is known by the parser, it must be supported
                // here.
                throw new ProcessDefinitionDeclarationException(new UnsupportedActionException(wsdlOperation,
                        annotatedOperation.getClass().getSimpleName()));
            }
        }

        return operations;
    }
    
}
