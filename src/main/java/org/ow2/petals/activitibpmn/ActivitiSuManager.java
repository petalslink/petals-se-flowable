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
package org.ow2.petals.activitibpmn;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.servicedesc.ServiceEndpoint;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.converter.util.InputStreamProvider;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.StartEvent;
import org.activiti.bpmn.model.UserTask;
import org.activiti.engine.RepositoryService;
import org.activiti.engine.impl.util.io.InputStreamSource;
import org.activiti.engine.repository.Deployment;
import org.activiti.engine.repository.DeploymentBuilder;
import org.activiti.engine.repository.ProcessDefinition;
import org.ow2.petals.activitibpmn.exception.IncoherentProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.InvalidVersionDeclaredException;
import org.ow2.petals.activitibpmn.exception.NoProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.UnexistingProcessFileException;
import org.ow2.petals.activitibpmn.operation.ActivitiOperation;
import org.ow2.petals.activitibpmn.operation.CompleteUserTaskOperation;
import org.ow2.petals.activitibpmn.operation.StartEventOperation;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.api.configuration.ConfigurationExtensions;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.jbidescriptor.generated.Provides;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.ow2.petals.component.framework.su.ServiceUnitDataHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


/**
 * @author Bertrand ESCUDIE - Linagora
 */
public class ActivitiSuManager extends AbstractServiceUnitManager {

    private RepositoryService repS;

    /**
     * The process definitions embedded into the service unit
     * 
     * @author Christophe DENEUX - Linagora
     * 
     */
    private class EmbeddedProcessDefinitions {
        /**
         * The process definitions of the service unit
         */
        protected Set<EmbeddedProcessDefinition> processDefinitions = new HashSet<EmbeddedProcessDefinition>();

        /**
         * The tenant identifier of embedded processes
         */
        protected String tenantId;

        /**
         * The category identifier of embedded processes
         */
        protected String categoryId;

        public EmbeddedProcessDefinitions(final String tenantId, final String categoryId) {
            super();
            this.tenantId = tenantId;
            this.categoryId = categoryId;
        }

        /**
         * <p>
         * Log the embedded process definitions for debug purpose
         * </p>
         * <p>
         * <b>Note</b>: For performance reasons, the caller is responsible to check the log level before to invoke this
         * method.
         * </p>
         * 
         * @param logger
         *            The logger to use
         */
        public void debug(final Logger logger) {
            logger.fine("Processes configuration");
            logger.fine("tenantId: " + tenantId);
            logger.fine("categoryId: " + categoryId);
            logger.fine(this.processDefinitions.size() + " process(es):");
            for (final EmbeddedProcessDefinition processDefinition : this.processDefinitions) {
                logger.fine("   - file name: " + processDefinition.processFileName);
                logger.fine("   - version: " + processDefinition.version);
            }
        }
    }
	
    /**
     * A process definition embedded into the service unit
     * 
     * @author Bertrand ESCUDIE - Linagora
     * 
     */
    private class EmbeddedProcessDefinition {
        protected final String processFileName;

        protected final int version;
		
        public EmbeddedProcessDefinition(final String processFileName, final int version) {
			this.processFileName = processFileName;
			this.version = version;
		}
	}	

	/**
	 * Default constructor.
	 * @param component the ACTIVITI component
	 */
    public ActivitiSuManager(final AbstractComponent component) {
		super(component);
        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("Start ActivitiSuManager.ActivitiSUManager()");
		}
        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("End ActivitiSuManager.ActivitiSUManager()");
		}
	}
	
    /**
     * {@inheritDoc}
     */
    @Override
    protected void doDeploy(final String serviceUnitName, final String suRootPath, final Jbi jbiDescriptor)
    throws PEtALSCDKException {
        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("Start ActivitiSuManager.doDeploy(SU =" + serviceUnitName + ")");
		}
        // Get the Activiti RepositoryService
        // TODO: What to do if repS is null ?
        if (this.repS == null) {
            this.repS = ((ActivitiSE) this.component).getProcessEngine().getRepositoryService();
        }
		
		// Check the JBI descriptor
		if( jbiDescriptor == null || jbiDescriptor.getServices() == null
                || jbiDescriptor.getServices().getProvides() == null
                || jbiDescriptor.getServices().getProvides().size() == 0) {
			throw new PEtALSCDKException( "Invalid JBI descriptor: it does not contain a 'provides' section." );
        }

		//TODO Manage consume for Activiti Petals Task
        if (jbiDescriptor.getServices().getConsumes().size() != 0) {
            throw new PEtALSCDKException( "Invalid JBI descriptor: 'Consumes' sections are not till supported by this component." );
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
	    // Deploy the processes from the file bpmn20.xml described in jbi.xml of the SU
        final Map<String, ProcessDefinition> processDefinitions = this.deployActivitiProcess(provides, suRootPath);

		// get the serviceEndPoint Name  of the Provides
        final String edptName = provides.getEndpointName();
	    // Get the endpoint of the provides
        final ServiceEndpoint serviceEndpoint = this.getEndpointsForServiceUnit(serviceUnitName).iterator().next();
        // get the Wsdl
        final Document wsdlDocument = this.getServiceDescription(serviceEndpoint);
        // Get the EptOperation and ActivitiOperation mapping and store in the eptOperationToActivitiOperation Map  
        this.getActivitiOperationMapping(wsdlDocument, edptName, processDefinitions);
        
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
            // Get the service end point associated to the service Unit Name
            final ServiceEndpoint serviceEndpoint = this.getEndpointsForServiceUnit(serviceUnitName).iterator().next();
            // set the serviceEndPoint Name of the Provides
            final String edptName = serviceEndpoint.getEndpointName();
            // Remove the ActivitiOperation in the map with the corresponding end-point
            ((ActivitiSE) this.component).removeActivitiOperation(edptName);

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

    private EmbeddedProcessDefinitions loadProcessDefinitions(final Provides provides) throws PEtALSCDKException {

        // Get the SU Data handler
        final ServiceUnitDataHandler suDataHandler = this.getSUDataHandlerForProvides(provides);
        if (suDataHandler == null) {
            throw new PEtALSCDKException(
                    "Error while processing the JBI descriptor in the component. The SU data handler was null.");
        }

        // Get the extension configuration for the Activiti process(es) to be deployed from the SU jbi.xml
        final ConfigurationExtensions extensions = suDataHandler.getConfigurationExtensions(provides);
        if (extensions == null) {
            throw new PEtALSCDKException("Invalid JBI descriptor: it does not contain any component extension.");
        }

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

        final EmbeddedProcessDefinitions embeddedProcessDefinitions = new EmbeddedProcessDefinitions(tenantId,
                categoryId);

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
            int nbProcesses = 1;
            do {
                processFileName = extensions.get(ActivitiSEConstants.PROCESS_FILE + nbProcesses);
                versionStr = extensions.get(ActivitiSEConstants.VERSION + nbProcesses);
                if (nbProcesses == 1 && processFileName == null && versionStr == null) {
                    throw new NoProcessDefinitionDeclarationException();
                } else if (processFileName != null && versionStr != null) {
                    this.registerEmbeddedProcessDefinition(processFileName, versionStr, embeddedProcessDefinitions);
                    nbProcesses++;
                }
            } while (processFileName != null && versionStr != null);
        } else if (processFileName != null && versionStr != null) {
            // One process description
            this.registerEmbeddedProcessDefinition(processFileName, versionStr, embeddedProcessDefinitions);
        } else {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        }

        if (this.logger.isLoggable(Level.FINE)) {
            embeddedProcessDefinitions.debug(this.logger);
        }

        return embeddedProcessDefinitions;
    }

    /**
     * <p>
     * Register an embedded process definition of the SU from its raw information
     * </p>
     * <p>
     * Does nothing if <code>processFileName</code> and <code>versionStr</code> are <code>null</code> in the same time.
     * </p>
     * 
     * @param processFileName
     *            The file name of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @param versionStr
     *            The version of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @throws PEtALSCDKException
     *             A raw data of the SU JBI descriptor is invalid
     */
    private void registerEmbeddedProcessDefinition(final String processFileName, final String versionStr,
            final EmbeddedProcessDefinitions embeddedProcessDefinitions) throws PEtALSCDKException {

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

        embeddedProcessDefinitions.processDefinitions.add(new EmbeddedProcessDefinition(processFileName, version));

    }

    /**
     * deploy in Activiti DB the process if it doesn't exist Process deployment is characterized in Activiti Database by
     * processName / tenantId / categoryId / version tenantId allows to have several instance of the same process model
     * in different contexts: different owner, assignee, group .... categoryId allows to manage process lifeCycle: Dev,
     * PreProd, Prod ... version allows to manage several versions The first time a process with a particular key is
     * deployed, version 1 is assigned. For all subsequent deployments of process definitions with the same key, the
     * version will be set 1 higher then the maximum currently deployed version.
     * 
     * @param provides
     *            - the provides that the SU expose
     * @param suRootPath
     *            path of the process file bpmn20.xml
     * @return a Map<String processKey,ProcessDefinition> of the corresponding processes described in the SU
     * @throws PEtALSCDKException
     */
    private Map<String, ProcessDefinition> deployActivitiProcess(final Provides provides, final String suRootPath)
            throws PEtALSCDKException {

        final Map<String, ProcessDefinition> processDefinitionsDeployed = new HashMap<String, ProcessDefinition>();

        // deploy the processes
        final EmbeddedProcessDefinitions processDefinitions = this.loadProcessDefinitions(provides);
        final Iterator<EmbeddedProcessDefinition> iterator = processDefinitions.processDefinitions.iterator();
		while (iterator.hasNext() ){
            final EmbeddedProcessDefinition process = iterator.next();
			
            // Check that the process is not already deployed. If it exists do not deploy it again then returns its
            // ProcessDefinition.
            // This allow to deploy the same SU (process.bpmn20.xml) on several petals-se-activitibpmn for high
            // availability, ie. to create several service endpoints for the same process/tenantId/categoryId/version on
            // different Petals ESB container.
            final List<ProcessDefinition> processDefinitionSearchList = this.repS.createProcessDefinitionQuery()
                    .processDefinitionResourceName(process.processFileName)
                    .processDefinitionCategory(processDefinitions.categoryId)
                    .processDefinitionTenantId(processDefinitions.tenantId).processDefinitionVersion(process.version)
                    .list();
	 		    
		    if (processDefinitionSearchList == null || processDefinitionSearchList.size() == 0) {
                final DeploymentBuilder db = this.repS.createDeployment();

                // Characterize the deployment with processFileName / tenantId / categoryId
				db.name(process.processFileName);
                db.tenantId(processDefinitions.tenantId);
                db.category(processDefinitions.categoryId);

				// add the process definition from file: file Name, file Path 
                final File processFile = new File(suRootPath, process.processFileName);
                try {
                    final FileInputStream bpmnInputFile = new FileInputStream(processFile);
                    try {
                        final InputStreamProvider bpmnInputStreamSource = new InputStreamSource(bpmnInputFile);

                        // add BPMN Model in order to change the category in the Process definition
                        // that is indeed derived from the targetNameSpace of the bpmn20.xml file
                        final org.activiti.bpmn.model.BpmnModel bpmnModel = new BpmnXMLConverter().convertToBpmnModel(
                                bpmnInputStreamSource, false, false);
                        bpmnModel.setTargetNamespace(processDefinitions.categoryId);

                        db.addBpmnModel(process.processFileName, bpmnModel);

                        // TODO manage the assignee according with su jbi descriptor

                        // TODO Manage the process suspension State be careful of multi SU deployement for the same
                        // process

                        // Do not use db.enableDuplicateFiltering(); with management of tenantId and CategoryId
                        final Deployment deployment = db.deploy();

                        // Set processDefinition
                        final ProcessDefinition processDefinition = this.repS.createProcessDefinitionQuery()
                                .deploymentId(deployment.getId()).list().get(0);
                        processDefinitionsDeployed.put(processDefinition.getKey(), processDefinition);

                        // TODO TO TEST Set Version of processDefinition
                        // TEST avec createNativeDeploymentQuery sur Deployment ce qui suit change la version du modele
                        // non du peloiement
                        /**
                         * Model model = repS.createModelQuery().deploymentId(deployment.getId())
                         * .latestVersion().modelTenantId(tenantId).modelCategory(categoryId).list().get(0);
                         * model.setVersion( Integer.valueOf(process.version));
                         */
                        if (this.logger.isLoggable(Level.INFO))
                            this.logger.info("The BPMN process " + process.processFileName + " version: "
                                    + process.version
                                    + " is succesfully deployed");
                    } finally {
                        try {
                            bpmnInputFile.close();
                        } catch (final IOException e) {
                            this.logger
                                    .log(Level.WARNING, "Unable to close BPMN definition file ''. Error skiped !", e);
                        }
                    }

				} catch (final FileNotFoundException e) {
                    throw new UnexistingProcessFileException(processFile.getAbsolutePath(), e);
                }

			}
			else {
                if (this.logger.isLoggable(Level.INFO))
                    this.logger.info("The BPMN process: " + process.processFileName + " version: " + process.version
                            + " is already deployed");
				// Set processDefinition
                final ProcessDefinition processDefinition = processDefinitionSearchList.get(0);
                processDefinitionsDeployed.put(processDefinition.getKey(), processDefinition);
			}
		}

		// Log the processDefinitionList
        if (this.logger.isLoggable(Level.FINE)) {
            int i = 0;
            for (final ProcessDefinition processDefinition : processDefinitionsDeployed.values()) {
                this.logger.fine("Process definition #" + i);
                this.logger.fine("   - Id            = " + processDefinition.getId());
                this.logger.fine("   - Category      = " + processDefinition.getCategory());
                this.logger.fine("   - Name          = " + processDefinition.getName());
                this.logger.fine("   - Key           = " + processDefinition.getKey());
                this.logger.fine("   - Version       = " + processDefinition.getVersion());
                this.logger.fine("   - Deployemnt Id = " + processDefinition.getDeploymentId());
                this.logger.fine("   - ResourceName  = " + processDefinition.getResourceName());
                this.logger.fine("   - TenantId      = " + processDefinition.getTenantId());
			}
		}
		
        return processDefinitionsDeployed;
    }


	/**
	 * Read the Wsdl associated with the provides in order to get the EptOperation and ActivitiOperation mapping
	 * Store the mapping in the eptOperationToActivitiOperation Maps
 	 * 
	 * @param wsdlDocument - the Wsdl corresponding to the provides
	 * @param edptName - the endpoint Name
	 * @param processDefinitions - the Map of Activiti processKey and processDefintion
	 * @return void
	 * @throws PEtALSCDKException 
	 */
    private void getActivitiOperationMapping(final Document wsdlDocument, final String edptName,
            final Map<String, ProcessDefinition> processDefinitions) throws PEtALSCDKException {

        wsdlDocument.getDocumentElement().normalize();
        // Get the node "wsdl:binding"
        final Node binding = wsdlDocument.getElementsByTagNameNS("http://schemas.xmlsoap.org/wsdl/", "binding").item(0);
        // Get the list of nodes "wsdl:operation"
        final NodeList operations = ((Element) binding).getElementsByTagNameNS("http://schemas.xmlsoap.org/wsdl/",
                "operation");

        for (int j = 0; j < operations.getLength(); j++) {
            final Node operation = operations.item(j);
            // set the eptAndOperation
            final EptAndOperation eptAndOperation = new EptAndOperation(edptName,
                    ((Element) operation).getAttribute("name"));
            // Get the node "bpmn:operation"
            final Node bpmnOperation = ((Element) operation).getElementsByTagNameNS(
                    "http://petals.ow2.org/se/Activitibpmn/1.0", "operation").item(0);
            // set the bpmnProcessKey
            final String bpmnProcessKey = ((Element) bpmnOperation).getAttribute("bpmnProcess");
            // Test the bpmnProcess value with processKey of the Map processDefinitions and set processDefinitionId
            if (!processDefinitions.containsKey(bpmnProcessKey)) {
                throw new PEtALSCDKException("Malformed Wsdl: business process Key = " + bpmnProcessKey
                        + " does not correspond to any Business Process file of the SU ");
            }
            final String processDefinitionId = processDefinitions.get(bpmnProcessKey).getId();
            // set the bpmnAction
            final String bpmnAction = ((Element) bpmnOperation).getAttribute("bpmnAction");

            // Get the node "bpmn:processId" and its message
            final Node processId = ((Element) operation).getElementsByTagNameNS(
                    "http://petals.ow2.org/se/Activitibpmn/1.0", "processId").item(0);
            final Properties bpmnProcessId = new Properties();
            if (processId != null) {
                // set the processId properties
                final String inMsgAttr = ((Element) processId).getAttribute("inMsg");
                if (inMsgAttr != null && !inMsgAttr.isEmpty()) {
                    bpmnProcessId.put("inMsg", inMsgAttr);
                }

                final String outMsgAttr = ((Element) processId).getAttribute("outMsg");
                if (outMsgAttr != null && !outMsgAttr.isEmpty()) {
                    bpmnProcessId.put("outMsg", outMsgAttr);
                }

                final String faultMsgAttr = ((Element) processId).getAttribute("faultMsg");
                if (faultMsgAttr != null && !faultMsgAttr.isEmpty()) {
                    bpmnProcessId.put("faultMsg", faultMsgAttr);
                }
            }

            // Get the node "bpmn:userId" and test it has always an InMsg attribute
            final Node userId = ((Element) operation).getElementsByTagNameNS(
                    "http://petals.ow2.org/se/Activitibpmn/1.0", "userId").item(0);
            final Properties bpmnUserId = new Properties();
            if (userId != null) {
                // set the bpmnUserId properties
                final String inMsgAttr = ((Element) userId).getAttribute("inMsg");
                if (inMsgAttr != null && !inMsgAttr.isEmpty()) {
                    bpmnUserId.put("inMsg", inMsgAttr);
                }

                final String outMsgAttr = ((Element) userId).getAttribute("outMsg");
                if (outMsgAttr != null && !outMsgAttr.isEmpty()) {
                    bpmnUserId.put("outMsg", outMsgAttr);
                }

                final String faultMsgAttr = ((Element) userId).getAttribute("faultMsg");
                if (faultMsgAttr != null && !faultMsgAttr.isEmpty()) {
                    bpmnUserId.put("faultMsg", faultMsgAttr);
                }
            }

            // Get the list of nodes "bpmn:variable"
            final NodeList bpmnVariableList = ((Element) operation).getElementsByTagNameNS(
                    "http://petals.ow2.org/se/Activitibpmn/1.0", "variable");
            final Properties bpmnVarInMsg = new Properties();
            final Properties outMsgBpmnVar = new Properties();
            final Properties faultMsgBpmnVar = new Properties();
            final Set<String> bpmnVarList = new HashSet<String>();
            for (int k = 0; k < bpmnVariableList.getLength(); k++) {
                final Node bpmnVariable = bpmnVariableList.item(k);
                // test name declaration of variable
                final String bpmnAttr = ((Element) bpmnVariable).getAttribute("bpmn");
                if (bpmnAttr == null) {
                    throw new PEtALSCDKException(
                            "Malformed Wsdl: bpmn:variable declared with no bpmn name in wsdl:operation: "
                                    + eptAndOperation.getOperationName());
                }
                // test unicity of declared bpmnVariable
                if (bpmnVarList.contains(bpmnAttr)) {
                    throw new PEtALSCDKException("Malformed Wsdl: bpmn:variable bpmn = " + bpmnAttr
                            + " declared twice in wsdl:operation: " + eptAndOperation.getOperationName());
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

            // get the bpmn variable types from BpmnModel
            final BpmnModel model = this.repS.getBpmnModel(processDefinitionId);
            // getting list process from model including tasks
            // TODO: I'm not sure that the concurrent hash map is required. A simple HashMap sould be sufficient
            final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType = new ConcurrentHashMap<String, org.activiti.bpmn.model.FormProperty>();
            final List<org.activiti.bpmn.model.Process> processes = model.getProcesses();
            List<org.activiti.bpmn.model.FormProperty> formPropertyList = null;
            boolean found = false;
            final String bpmnActionType = ((Element) bpmnOperation).getAttribute("bpmnActionType");
            if (StartEventOperation.BPMN_ACTION_TYPE.equals(bpmnActionType)) {
                // search form Property for the Start Event: bpmnAction
                outerloop: for (final org.activiti.bpmn.model.Process process : processes) {
                    for (final org.activiti.bpmn.model.FlowElement flowElt : process.getFlowElements()) {
                        // search the Start Event: bpmnAction
                        if ((flowElt instanceof StartEvent) && (flowElt.getId().equals(bpmnAction))) {
                            StartEvent startEvent = (StartEvent) flowElt;
                            formPropertyList = startEvent.getFormProperties();
                            found = true;
                            break outerloop;
                        }
                    }
                }
            } else if (CompleteUserTaskOperation.BPMN_ACTION_TYPE.equals(bpmnActionType)) {
                // search form Property for the User Task: bpmnAction
                outerloop: for (final org.activiti.bpmn.model.Process process : processes) {
                    for (final org.activiti.bpmn.model.FlowElement flowElt : process.getFlowElements()) {
                        // search the Start Event: bpmnAction
                        if ((flowElt instanceof UserTask) && (flowElt.getId().equals(bpmnAction))) {
                            UserTask userTask = (UserTask) flowElt;
                            formPropertyList = userTask.getFormProperties();
                            found = true;
                            break outerloop;
                        }
                    }
                }
            } else {
                this.logger.warning("Unsupported BPMN action type '" + bpmnActionType + "'. Skipped");
            }
            if (!found) {
                throw new PEtALSCDKException("Malformed Wsdl: BpmnAction : " + bpmnAction
                        + ", does not exist in Activiti process : " + processDefinitionId);
            }
            if (formPropertyList != null && formPropertyList.size() > 0) {
                for (final org.activiti.bpmn.model.FormProperty formPropertie : formPropertyList) {
                    // add the FormProperty to the Map <bpmnvar, FormProperty>
                    bpmnVarType.put(formPropertie.getId(), formPropertie);
                }
            }
            // TODO test the existence of the declared bpmn:variables
            for (final String bpmnVar : bpmnVarList) {
                if (!bpmnVarType.containsKey(bpmnVar)) {
                    throw new PEtALSCDKException("Malformed Wsdl: bpmn:variable : " + bpmnVar
                            + ", does not exist in Activiti process : " + processDefinitionId);
                }
            }

            // create the right ActivitiOperation according to the bpmnActionType
            final ActivitiOperation activitiOperation;
            if (StartEventOperation.BPMN_ACTION_TYPE.equals(bpmnActionType)) {
                activitiOperation = new StartEventOperation(processDefinitionId, bpmnProcessKey, bpmnAction,
                        bpmnProcessId, bpmnUserId, bpmnVarInMsg, outMsgBpmnVar, faultMsgBpmnVar, bpmnVarType,
                        this.logger);
            } else if (CompleteUserTaskOperation.BPMN_ACTION_TYPE.equals(bpmnActionType)) {
                activitiOperation = new CompleteUserTaskOperation(processDefinitionId, bpmnProcessKey, bpmnAction,
                        bpmnProcessId, bpmnUserId, bpmnVarInMsg, outMsgBpmnVar, faultMsgBpmnVar, bpmnVarType,
                        this.logger);
            } else {
                throw new PEtALSCDKException("Malformed Wsdl: Unauthorized BpmnActionType :" + bpmnActionType
                        + " in wsdl operation = " + ((Element) operation).getAttribute("name"));
            }

            // Store the ActivitiOperation in the map with the corresponding end-point and Operation
            ((ActivitiSE) this.component).registerActivitiOperation(eptAndOperation, activitiOperation);
        }

        // List the map of (end-point and Operation) and ActivitiOperation
        ((ActivitiSE) this.component).logEptOperationToActivitiOperation(this.logger, Level.FINEST);
    }
    
}
